{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module HBDoc.Serialize.Edit
  ( insertBlockAfter
  , insertBlockBefore
  , moveBlock
  , deleteBlock
  , resequenceBlocks
  ) where

import Control.Monad (forM_, when)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Ae

import qualified Hasql.Session as Ses
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs
import Hasql.Pool (Pool, UsageError, use)

import HBDoc.Core.Attributes
import HBDoc.Core.BlockKind
import HBDoc.Core.Types
import HBDoc.Core.Validate

import qualified HBDoc.Serialize.Statements as St

useTx :: Pool -> Tx.Transaction a -> IO (Either UsageError a)
useTx pool trx =
  use pool (Txs.transaction Txs.Serializable Txs.Write trx)

type ApiResult a = Either String a

errApi :: String -> ApiResult a
errApi errTxt =
  Left errTxt

okApi :: a -> ApiResult a
okApi val =
  Right val


insertBlockAfter ::
  Pool ->
  Int32 ->          -- actor user uid
  Int32 ->          -- document uid (for permission / attachments)
  Int64 ->          -- anchor block uid
  Block spec ->     -- subtree root to insert
  IO (ApiResult Int64)
insertBlockAfter pool actor docId blkAnchor blkNew = do
  case validateInsertableBlock blkNew of
    Left errTxt ->
      pure (errApi (T.unpack errTxt))

    Right () -> do
      trxRes <- useTx pool $ do
        canEdit <- Tx.statement (actor, "edit" :: Text, docId) St.qCanUser
        if not canEdit
          then pure (errApi "forbidden:edit")
          else do
            newUid <- insertBlockAfterTx actor docId blkAnchor blkNew
            pure (okApi newUid)

      finalizeTx "insertBlockAfter" trxRes

insertBlockBefore ::
  Pool ->
  Int32 ->          -- actor user uid
  Int32 ->          -- document uid (for permission / attachments)
  Int64 ->          -- anchor block uid
  Block spec ->     -- subtree root to insert
  IO (ApiResult Int64)
insertBlockBefore pool actor docId blkAnchor blkNew = do
  case validateInsertableBlock blkNew of
    Left errTxt ->
      pure (errApi (T.unpack errTxt))

    Right () -> do
      trxRes <- useTx pool $ do
        canEdit <- Tx.statement (actor, "edit" :: Text, docId) St.qCanUser
        if not canEdit
          then pure (errApi "forbidden:edit")
          else do
            newUid <- insertBlockBeforeTx actor docId blkAnchor blkNew
            pure (okApi newUid)

      finalizeTx "insertBlockBefore" trxRes

moveBlock ::
  Pool ->
  Int32 ->          -- actor user uid
  Int32 ->          -- document uid (for permission)
  Int64 ->          -- block uid
  Int64 ->          -- new parent uid
  Maybe Int64 ->    -- after block uid
  Maybe Int64 ->    -- before block uid
  IO (ApiResult ())
moveBlock pool actor docId blkUid newParentUid mbAfterUid mbBeforeUid
  | mbAfterUid /= Nothing && mbBeforeUid /= Nothing =
      pure (errApi "moveBlock: only one of after/before may be provided")
  | otherwise = do
      trxRes <- useTx pool $ do
        canEdit <- Tx.statement (actor, "edit" :: Text, docId) St.qCanUser
        if not canEdit
          then pure (errApi "forbidden:edit")
          else do
            Tx.statement
              ( blkUid
              , newParentUid
              , mbAfterUid
              , mbBeforeUid
              , Just actor
              )
              St.qMoveBlock
            pure (okApi ())

      finalizeTx "moveBlock" trxRes

deleteBlock ::
  Pool ->
  Int32 ->      -- actor user uid
  Int32 ->      -- document uid (for permission)
  Int64 ->      -- block uid
  IO (ApiResult ())
deleteBlock pool actor docId blkUid = do
  trxRes <- useTx pool $ do
    canEdit <- Tx.statement (actor, "edit" :: Text, docId) St.qCanUser
    if not canEdit
      then pure (errApi "forbidden:edit")
      else do
        Tx.statement (blkUid, Just actor) St.qDeleteBlock
        pure (okApi ())

  finalizeTx "deleteBlock" trxRes

resequenceBlocks ::
  Pool ->
  Int32 ->          -- actor user uid
  Int32 ->          -- document uid (for permission)
  Maybe Int64 ->    -- parent block uid; Nothing means root-level children
  IO (ApiResult ())
resequenceBlocks pool actor docId mbParentUid = do
  trxRes <- useTx pool $ do
    canEdit <- Tx.statement (actor, "edit" :: Text, docId) St.qCanUser
    if not canEdit
      then pure (errApi "forbidden:edit")
      else do
        Tx.statement (docId, mbParentUid, Just actor) St.qResequenceBlocks
        pure (okApi ())

  finalizeTx "resequenceBlocks" trxRes

-- ---------------------------------------------------------------------
-- Internal tree-writing helpers
-- ---------------------------------------------------------------------

insertBlockAfterTx ::
  Int32 ->
  Int32 ->
  Int64 ->
  Block spec ->
  Tx.Transaction Int64
insertBlockAfterTx actor docId blkAnchor blkNew = do
  newUid <-
    Tx.statement
      ( blkAnchor
      , kindCode
      , kindArg
      , contentBk blkNew
      , semJson
      , attrsJson
      , provenanceJson
      , actor
      )
      St.qInsertBlockAfter

  maybeLinkBlockMedia actor docId newUid blkNew
  forM_ blkNew.childrenBk (appendSubtreeTx actor docId (Just newUid))
  pure newUid
  where
    (kindCode, kindArg) = kindToDb blkNew.kindBk
    semJson = fmap Ae.toJSON blkNew.semBk
    attrsJson = Just (Ae.toJSON blkNew.attribsBk)
    provenanceJson = fmap Ae.toJSON blkNew.provenanceBk

insertBlockBeforeTx ::
  Int32 ->
  Int32 ->
  Int64 ->
  Block spec ->
  Tx.Transaction Int64
insertBlockBeforeTx actor docId blkAnchor blkNew = do
  newUid <-
    Tx.statement
      ( blkAnchor
      , kindCode
      , kindArg
      , contentBk blkNew
      , semJson
      , attrsJson
      , provenanceJson
      , actor
      )
      St.qInsertBlockBefore

  maybeLinkBlockMedia actor docId newUid blkNew
  forM_ blkNew.childrenBk (appendSubtreeTx actor docId (Just newUid))
  pure newUid
  where
    (kindCode, kindArg) = kindToDb blkNew.kindBk
    semJson = fmap Ae.toJSON blkNew.semBk
    attrsJson = Just (Ae.toJSON blkNew.attribsBk)
    provenanceJson = fmap Ae.toJSON blkNew.provenanceBk

appendSubtreeTx ::
  Int32 ->
  Int32 ->
  Maybe Int64 ->
  Block spec ->
  Tx.Transaction Int64
appendSubtreeTx actor docId parentUid blk = do
  newUid <-
    Tx.statement
      ( docId
      , parentUid
      , kindCode
      , kindArg
      , contentBk blk
      , semJson
      , attrsJson
      , provenanceJson
      , actor
      )
      St.qAppendChild

  maybeLinkBlockMedia actor docId newUid blk
  forM_ blk.childrenBk (appendSubtreeTx actor docId (Just newUid))
  pure newUid
  where
    (kindCode, kindArg) = kindToDb blk.kindBk
    semJson = fmap Ae.toJSON blk.semBk
    attrsJson = Just (Ae.toJSON blk.attribsBk)
    provenanceJson = fmap Ae.toJSON blk.provenanceBk

maybeLinkBlockMedia ::
  Int32 ->
  Int32 ->
  Int64 ->
  Block spec ->
  Tx.Transaction ()
maybeLinkBlockMedia actor docId blkUid blk =
  case mediaForBlock blk of
    Nothing ->
      pure ()

    Just media ->
      case (media.imageKeyMa, media.imageNameMa) of
        (Just keyTxt, Just fileNameTxt) -> do
          let ct = guessContentType fileNameTxt
          attUid <-
            Tx.statement
              ( docId
              , fileNameTxt
              , ct
              , 0
              , keyTxt
              , ""
              , actor
              )
              St.qInsertAttachment
          Tx.statement (blkUid, attUid, actor) St.qLinkBlockAttachment
          pure ()

        _ ->
          pure ()

mediaForBlock :: Block spec -> Maybe MediaAttrs
mediaForBlock blk =
  case blk.kindBk of
    ImageKB -> blk.attribsBk.mediaAb
    FigureKB -> blk.attribsBk.mediaAb
    _ -> Nothing

validateInsertableBlock :: Block spec -> Either Text ()
validateInsertableBlock blk
  | blk.kindBk == ContainerKB =
      Left "ContainerKB cannot be inserted as ordinary content; it is reserved for the HBDoc root."
  | otherwise =
      let issues = validateBlockTree blk
      in if hasErrors issues
           then Left (T.intercalate "\n" (renderIssues issues))
           else Right ()

kindToDb :: KindBlk -> (Text, Maybe Text)
kindToDb = \case
  ContainerKB -> ("container", Nothing)
  HeadingKB -> ("heading", Nothing)
  ParagraphKB -> ("paragraph", Nothing)
  ListKB -> ("list", Nothing)
  ListItemKB -> ("list_item", Nothing)
  QuoteKB -> ("quote", Nothing)
  CodeKB -> ("code", Nothing)
  TableKB -> ("table", Nothing)
  TableRowKB -> ("table_row", Nothing)
  TableCellKB -> ("table_cell", Nothing)
  FigureKB -> ("figure", Nothing)
  ImageKB -> ("image", Nothing)
  RuleKB -> ("rule", Nothing)
  NoteKB FootnoteNK -> ("note", Just "footnote")
  NoteKB EndnoteNK -> ("note", Just "endnote")
  ConversationKB -> ("conversation", Nothing)
  MessageKB -> ("message", Nothing)
  CustomKB tagTxt -> ("custom", Just tagTxt)

guessContentType :: Text -> Text
guessContentType fileNameTxt
  | ".png" `T.isSuffixOf` lower = "image/png"
  | ".jpg" `T.isSuffixOf` lower = "image/jpeg"
  | ".jpeg" `T.isSuffixOf` lower = "image/jpeg"
  | ".gif" `T.isSuffixOf` lower = "image/gif"
  | ".webp" `T.isSuffixOf` lower = "image/webp"
  | ".svg" `T.isSuffixOf` lower = "image/svg+xml"
  | otherwise = "application/octet-stream"
  where
    lower = T.toLower fileNameTxt

finalizeTx :: String -> Either UsageError (ApiResult a) -> IO (ApiResult a)
finalizeTx labelTxt trxRes =
  case trxRes of
    Left usageErr ->
      pure (errApi ("@[" <> labelTxt <> "] transaction error: " <> show usageErr))
    Right apiRes ->
      pure apiRes