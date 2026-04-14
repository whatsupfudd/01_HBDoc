{-# LANGUAGE LambdaCase #-}
module HBDoc.Serialize.Write where

import Control.Monad (forM_, when)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Ae

import qualified Hasql.Session as Ses
import qualified Hasql.Statement as St
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs
import Hasql.Pool (Pool, use, UsageError)

import HBDoc.Core.Attributes
import HBDoc.Core.BlockKind
import HBDoc.Core.Types
import HBDoc.Core.Validate

import qualified HBDoc.Serialize.Statements as St
import HBDoc.Serialize.Types (SerializeInfo(..))
import HBDoc.Manage.Types (User(..))

-- | Run a transaction through the pool.
useTx :: Pool -> Tx.Transaction a -> IO (Either UsageError a)
useTx pool trx =
  use pool (Txs.transaction Txs.Serializable Txs.Write trx)

blockCount :: [Block spec] -> Int
blockCount blocksDoc =
  length blocksDoc + sum (map (blockCount . childrenBk) blocksDoc)

serializeDocument :: Pool -> SerializeInfo docSpec blkSpec -> IO (Either String (Either String Int64))
serializeDocument pool sInfo = do
  let doc = document sInfo
      validationIssues = validateHBDoc doc

  when (not (null validationIssues)) $
    putStrLn ("@[serializeDocument] validation issues: " <> show (length validationIssues))

  if hasErrors validationIssues
    then
      pure $
        Left $
          T.unpack (T.intercalate "\n" (renderIssues validationIssues))
    else do
      docIdRes <-
        case mbDocID sInfo of
          Just docId ->
            pure (Right docId)
          Nothing ->
            pure (Left "@[serializeDocument] no docId provided")

      case docIdRes of
        Left errTxt ->
          pure (Left errTxt)

        Right docId -> do
          userRes <- resolveUserByEmail pool (userName sInfo)
          case userRes of
            Left errTxt ->
              pure (Left errTxt)

            Right Nothing ->
              pure (Left ("@[serializeDocument] user not found: " <> T.unpack (userName sInfo)))

            Right (Just user) -> do
              trxRes <- useTx pool (serializeDocumentTx sInfo user docId)
              case trxRes of
                Left usageErr ->
                  pure (Left ("@[serializeDocument] transaction error: " <> show usageErr))
                Right apiRes ->
                  pure (Right apiRes)

serializeDocumentTx :: SerializeInfo docSpec blkSpec -> User -> Int32 -> Tx.Transaction (Either String Int64)
serializeDocumentTx sInfo user docId = do
  canEdit <- Tx.statement (user.uidUsr, "edit" :: Text, docId) St.qCanUser
  if not canEdit then
    pure (Left "forbidden:edit")
  else do
    (domFk, tierFk, stFk, mRes) <- Tx.statement docId St.qDocMeta
    mBlocked <- Tx.statement (domFk, tierFk, stFk, mRes) St.qPolicyBlockImport
    case mBlocked of
      Just True -> pure (Left "blocked_by_policy:import")
      _ -> do
        _attUid <-
          Tx.statement
            ( docId
            , originalName sInfo
            , contentType sInfo
            , size sInfo
            , key sInfo
            , shaHex sInfo
            , user.uidUsr
            )
            St.qInsertAttachment

        rootUid <- Tx.statement (docId, user.uidUsr) St.qEnsureRoot

        let rootBlk = rootBlkDc (document sInfo)

        case kindBk rootBlk of
          ContainerKB -> do
            forM_ (childrenBk rootBlk) (writeBlockTree user.uidUsr docId (Just rootUid))
            Tx.statement
              ( user.uidUsr
              , "doc.import.blocktree" :: Text
              , Just docId
              , Just "document" :: Maybe Text
              , Nothing :: Maybe Int32
              , Nothing :: Maybe Text
              )
              St.qAudit
            pure (Right rootUid)

          otherKind ->
            pure (Left $ T.unpack ("invalid_root_kind:" <> renderKindCode otherKind))


writeBlockTree :: Int32 -> Int32 -> Maybe Int64 -> Block spec -> Tx.Transaction Int64
writeBlockTree actor docId parentUid blk = do
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

  forM_ (childrenBk blk) (writeBlockTree actor docId (Just newUid))
  pure newUid
  where
    (kindCode, kindArg) = kindToDb (kindBk blk)
    semJson = fmap Ae.toJSON (semBk blk)
    attrsJson = Just (Ae.toJSON (attribsBk blk))
    provenanceJson = fmap Ae.toJSON (provenanceBk blk)

maybeLinkBlockMedia :: Int32 -> Int32 -> Int64 -> Block spec -> Tx.Transaction ()
maybeLinkBlockMedia actor docId blockUid blk =
  case mediaForBlock blk of
    Nothing ->
      pure ()

    Just media ->
      case (imageKeyMa media, imageNameMa media) of
        (Just keyTxt, Just fileNameTxt) ->
          let
            ct = guessContentType fileNameTxt
          in do
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
          -- The qLinkBlockAttachment SQL logic actually returns a void, but how to support that in Hasql.
          Tx.statement (blockUid, attUid, actor) St.qLinkBlockAttachment
          pure ()
        _ ->  pure ()

mediaForBlock :: Block spec -> Maybe MediaAttrs
mediaForBlock blk =
  case kindBk blk of
    ImageKB -> mediaAb (attribsBk blk)
    FigureKB -> mediaAb (attribsBk blk)
    _ -> Nothing

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

renderKindCode :: KindBlk -> Text
renderKindCode =
  fst . kindToDb

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

resolveUserByEmail :: Pool -> Text -> IO (Either String (Maybe User))
resolveUserByEmail pool emailTxt = do
  sessionRes <- use pool (Ses.statement emailTxt St.qGetUserByEmail)
  case sessionRes of
    Left errTxt ->
      pure (Left ("@[resolveUserByEmail] err: " <> show errTxt))
    Right userMb ->
      pure (Right userMb)