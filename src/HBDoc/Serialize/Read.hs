
module HBDoc.Serialize.Read
  ( DbDocInfo(..)
  , DbBlockInfo(..)
  , HBDocDb
  , BlockDb
  , loadDocumentLive
  , loadDocumentAtSeq
  , loadSubtreeLive
  , loadSubtreeAtSeq
  ) where

import Data.Int (Int32, Int64)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Data.Aeson as Ae
import qualified Hasql.Session as Ses
import qualified Hasql.Statement as St
import Hasql.Pool (Pool, use)

import HBDoc.Core.Attributes
import HBDoc.Core.BlockKind
import HBDoc.Core.ListSemantics
import HBDoc.Core.Provenance
import HBDoc.Core.Semantics
import HBDoc.Core.Types
import HBDoc.Core.Validate

import qualified HBDoc.Serialize.Statements as Dbs
import HBDoc.Manage.Types (DocDetail(..))


data DbDocInfo = DbDocInfo
  { uidDocDbi :: !Int32
  , asOfSeqDocDbi :: !(Maybe Int64)
  , latestVerUidDocDbi :: !(Maybe Int32)
  , latestVerNoDocDbi :: !(Maybe Int32)
  }
  deriving (Show, Eq)

data DbBlockInfo = DbBlockInfo
  { uidBlkDbi :: !Int64
  , parentUidBlkDbi :: !(Maybe Int64)
  , seqPosBlkDbi :: !Scientific
  , hasMoreBlkDbi :: !(Maybe Bool)
  }
  deriving (Show, Eq)


type HBDocDb = HBDoc DbDocInfo DbBlockInfo
type BlockDb = Block DbBlockInfo

loadDocumentLive :: Pool -> Int32 -> IO (Either String HBDocDb)
loadDocumentLive pool docId =
  loadDocumentAtSeq pool docId maxLiveSeq

loadDocumentAtSeq :: Pool -> Int32 -> Int64 -> IO (Either String HBDocDb)
loadDocumentAtSeq pool docId asOfSeq = do
  eDetail <- runStatement pool Dbs.qDocDetail docId
  case eDetail of
    Left err ->
      pure (Left err)

    Right Nothing ->
      pure (Left "document_not_found")

    Right (Just detail) -> do
      eRows <- runStatement pool Dbs.qDocumentBlocksAsOfSeq (docId, asOfSeq)
      case eRows of
        Left err ->
          pure (Left err)

        Right rows ->
          pure (buildDocument detail asOfSeq (V.toList rows))

loadSubtreeLive :: Pool -> Int64 -> IO (Either String BlockDb)
loadSubtreeLive pool blkUid =
  loadSubtreeAtSeq pool blkUid maxLiveSeq Nothing 0 Nothing

loadSubtreeAtSeq ::
  Pool ->
  Int64 ->          -- root block uid
  Int64 ->          -- as_of_seq
  Maybe Int32 ->    -- max_depth
  Int64 ->          -- offset
  Maybe Int64 ->    -- limit
  IO (Either String BlockDb)
loadSubtreeAtSeq pool blkUid asOfSeq maxDepth offset limitMb = do
  eRows <- runStatement pool Dbs.qGetSubtreeDfsAtSeq (blkUid, asOfSeq, maxDepth, offset, limitMb)
  case eRows of
    Left err ->
      pure (Left err)

    Right rows ->
      pure (buildSubtree (V.toList rows))

-- ---------------------------------------------------------------------
-- Assembly
-- ---------------------------------------------------------------------

buildDocument :: DocDetail -> Int64 -> [Dbs.BlockAtSeqRow] -> Either String HBDocDb
buildDocument detail rowsSeq rowsFlat = do
  rowsById <- buildRowMap rowsFlat
  childMap <- buildChildMap rowsFlat

  rootRow <- locateRootRow rowsFlat
  rootBlk <- buildBlockFromAtSeq rowsById childMap rootRow

  let doc =
        HBDoc
          { titleDc = titleDtl detail
          , formatDc = Nothing
          , metaDefDc = mempty
          , rootBlkDc = rootBlk
          , extraDc =
              DbDocInfo
                { uidDocDbi = uidDtl detail
                , asOfSeqDocDbi = Just rowsSeq
                , latestVerUidDocDbi = latestVerUidDtl detail
                , latestVerNoDocDbi = latestVerNoDtl detail
                }
          }

      issues = validateHBDoc doc

  if hasErrors issues
    then Left (T.unpack (T.intercalate "\n" (renderIssues issues)))
    else Right doc

buildSubtree :: [Dbs.BlockDfsRow] -> Either String BlockDb
buildSubtree rowsFlat = do
  rowsById <- buildSubtreeRowMap rowsFlat
  childMap <- buildSubtreeChildMap rowsFlat

  rootRow <-
    case rowsFlat of
      [] -> Left "subtree_not_found"
      (x : _) -> Right x

  blk <- buildBlockFromDfs rowsById childMap rootRow

  let issues = validateBlockTree blk
  if hasErrors issues
    then Left (T.unpack (T.intercalate "\n" (renderIssues issues)))
    else Right blk

-- ---------------------------------------------------------------------
-- Tree reconstruction for document-wide rows
-- ---------------------------------------------------------------------

buildRowMap :: [Dbs.BlockAtSeqRow] -> Either String (M.Map Int64 Dbs.BlockAtSeqRow)
buildRowMap rowsFlat =
  let pairs = [ (row.uidBlkBas, row) | row <- rowsFlat ]
  in if length pairs == M.size (M.fromList pairs)
       then Right (M.fromList pairs)
       else Left "duplicate_block_uid_in_document_rows"

buildChildMap :: [Dbs.BlockAtSeqRow] -> Either String (M.Map (Maybe Int64) [Int64])
buildChildMap rowsFlat =
  Right $
    M.map (sortOn fst >>> map snd) $
      M.fromListWith (++)
        [ (row.parentUidBas, [(row.seqPosBas, row.uidBlkBas)])
        | row <- rowsFlat
        ]
  where
    (>>>) = flip (.)

locateRootRow :: [Dbs.BlockAtSeqRow] -> Either String Dbs.BlockAtSeqRow
locateRootRow rowsFlat =
  case [ row | row <- rowsFlat, isNothing row.parentUidBas ] of
    [] ->
      Left "document_root_not_found"
    [row] ->
      Right row
    _ ->
      Left "multiple_document_roots_found"

buildBlockFromAtSeq ::
  M.Map Int64 Dbs.BlockAtSeqRow ->
  M.Map (Maybe Int64) [Int64] ->
  Dbs.BlockAtSeqRow ->
  Either String BlockDb
buildBlockFromAtSeq rowsById childMap row = do
  kind <- decodeKind row.kindCodeBas row.kindArgBas
  semVal <- decodeMaybeJson "semantics" row.semBas
  attrsVal <- decodeJson "attributes" row.attrsBas
  provenanceVal <- decodeMaybeJson "provenance" row.provenanceBas

  let childUids = M.findWithDefault [] (Just row.uidBlkBas) childMap
  childRows <- traverse lookupChild childUids
  childBlks <- traverse (buildBlockFromAtSeq rowsById childMap) childRows

  pure $
    Block
      { kindBk = kind
      , contentBk = row.contentBas
      , semBk = semVal
      , attribsBk = attrsVal
      , provenanceBk = provenanceVal
      , childrenBk = childBlks
      , extraBk =
          DbBlockInfo
            { uidBlkDbi = row.uidBlkBas
            , parentUidBlkDbi = row.parentUidBas
            , seqPosBlkDbi = row.seqPosBas
            , hasMoreBlkDbi = Nothing
            }
      }
  where
    lookupChild uid =
      case M.lookup uid rowsById of
        Nothing -> Left ("missing_child_row: " <> show uid)
        Just childRow -> Right childRow

-- ---------------------------------------------------------------------
-- Tree reconstruction for subtree DFS rows
-- ---------------------------------------------------------------------

buildSubtreeRowMap :: [Dbs.BlockDfsRow] -> Either String (M.Map Int64 Dbs.BlockDfsRow)
buildSubtreeRowMap rowsFlat =
  let pairs = [ (row.uidBlkBdr, row) | row <- rowsFlat ]
  in if length pairs == M.size (M.fromList pairs)
       then Right (M.fromList pairs)
       else Left "duplicate_block_uid_in_subtree_rows"

buildSubtreeChildMap :: [Dbs.BlockDfsRow] -> Either String (M.Map (Maybe Int64) [Int64])
buildSubtreeChildMap rowsFlat =
  Right $
    M.map (sortOn fst >>> map snd) $
      M.fromListWith (++)
        [ (row.parentUidBdr, [(row.seqPosBdr, row.uidBlkBdr)])
        | row <- rowsFlat
        ]
  where
    (>>>) = flip (.)

buildBlockFromDfs ::
  M.Map Int64 Dbs.BlockDfsRow ->
  M.Map (Maybe Int64) [Int64] ->
  Dbs.BlockDfsRow ->
  Either String BlockDb
buildBlockFromDfs rowsById childMap row = do
  kind <- decodeKind row.kindCodeBdr row.kindArgBdr
  semVal <- decodeMaybeJson "semantics" row.semBdr
  attrsVal <- decodeJson "attributes" row.attrsBdr
  provenanceVal <- decodeMaybeJson "provenance" row.provenanceBdr

  let
    childUids = M.findWithDefault [] (Just row.uidBlkBdr) childMap
  childRows <- traverse lookupChild childUids
  childBlks <- traverse (buildBlockFromDfs rowsById childMap) childRows

  pure $
    Block
      { kindBk = kind
      , contentBk = row.contentBdr
      , semBk = semVal
      , attribsBk = attrsVal
      , provenanceBk = provenanceVal
      , childrenBk = childBlks
      , extraBk =
          DbBlockInfo
            { uidBlkDbi = row.uidBlkBdr
            , parentUidBlkDbi = row.parentUidBdr
            , seqPosBlkDbi = row.seqPosBdr
            , hasMoreBlkDbi = Just row.hasMoreBdr
            }
      }
  where
    lookupChild uid =
      case M.lookup uid rowsById of
        Nothing -> Left ("missing_child_row: " <> show uid)
        Just childRow -> Right childRow

-- ---------------------------------------------------------------------
-- Decoding helpers
-- ---------------------------------------------------------------------

decodeKind :: Text -> Maybe Text -> Either String KindBlk
decodeKind kindCode kindArg =
  case kindCode of
    "container" -> Right ContainerKB
    "heading" -> Right HeadingKB
    "paragraph" -> Right ParagraphKB
    "list" -> Right ListKB
    "list_item" -> Right ListItemKB
    "quote" -> Right QuoteKB
    "code" -> Right CodeKB
    "table" -> Right TableKB
    "table_row" -> Right TableRowKB
    "table_cell" -> Right TableCellKB
    "figure" -> Right FigureKB
    "image" -> Right ImageKB
    "rule" -> Right RuleKB
    "conversation" -> Right ConversationKB
    "message" -> Right MessageKB

    "note" ->
      case kindArg of
        Just "footnote" -> Right (NoteKB FootnoteNK)
        Just "endnote" -> Right (NoteKB EndnoteNK)
        Just other -> Left ("invalid_note_kind_arg: " <> T.unpack other)
        Nothing -> Left "missing_note_kind_arg"

    "custom" ->
      case kindArg of
        Just tagTxt -> Right (CustomKB tagTxt)
        Nothing -> Left "missing_custom_kind_arg"

    _ ->
      Left ("unknown_kind_code: " <> T.unpack kindCode)

decodeJson :: Ae.FromJSON a => String -> Ae.Value -> Either String a
decodeJson label val =
  case Ae.fromJSON val of
    Ae.Error err ->
      Left ("failed_to_decode_" <> label <> ": " <> err)
    Ae.Success x ->
      Right x

decodeMaybeJson :: Ae.FromJSON a => String -> Maybe Ae.Value -> Either String (Maybe a)
decodeMaybeJson _ Nothing =
  Right Nothing
decodeMaybeJson label (Just val) =
  Just <$> decodeJson label val

-- ---------------------------------------------------------------------
-- Session helpers
-- ---------------------------------------------------------------------

runStatement :: Pool -> St.Statement a b -> a -> IO (Either String b)
runStatement pool stmt params = do
  res <- use pool (Ses.statement params stmt)
  case res of
    Left usageErr ->
      pure (Left (show usageErr))
    Right val ->
      pure (Right val)

maxLiveSeq :: Int64
maxLiveSeq = maxBound