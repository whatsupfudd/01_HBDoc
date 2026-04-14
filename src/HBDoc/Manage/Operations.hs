module HBDoc.Manage.Operations where

import Control.Monad (void)

import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import qualified Data.Text as T
import qualified Data.Vector as V

import Hasql.Session (statement)
import Hasql.Pool (Pool, use)
import qualified Hasql.Session as S

import qualified HBDoc.Serialize.Statements as St
import HBDoc.Manage.Types

type DataResult a = Either String a


-- Users/Auth --------------------------------------------------
resolveUser :: Pool -> Text -> IO (Either String (Maybe User))
resolveUser pool email = do
  r <- use pool (statement email St.qGetUserByEmail)
  case r of
    Left ue -> pure . Left $ "@[resolveUser] err: " <> show ue
    Right aValue -> pure $ Right aValue


canUser :: Pool -> Int32 -> Text -> Int32 -> IO (DataResult Bool)
canUser pool u perm d =
  use pool (statement (u, perm, d) St.qCanUser) <&> either (Left . show) Right


-- Categorisation --------------------------------------------------
listCategories :: Pool -> IO (DataResult (V.Vector Category))
listCategories pool =
  use pool (statement () St.qListCategories) <&> either (Left . show) Right


fetchNodes :: Pool -> Int32 -> IO (DataResult (V.Vector St.NodeOut))
fetchNodes pool taxoID =
  use pool (statement taxoID St.fetchNodesForTaxo) <&> either (Left . show) Right


-- Documents --------------------------------------------------
listDocs :: Pool -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Text -> Maybe Day -> Maybe UTCTime -> IO (DataResult (V.Vector DocRow))
listDocs pool dom st tier q limit offset =
  use pool (statement (dom, st, tier, q, limit, offset) St.qListDocs)
    <&> either (Left . show) Right


getDoc :: Pool -> Int32 -> IO (DataResult DocDetail)
getDoc pool docId = do
  r <- use pool (statement docId St.qDocDetail)
  case r of
    Left ue -> pure . Left $ show ue
    Right Nothing -> pure $ Left "@[getDoc] doc_not_found"
    Right (Just dd) -> pure $ Right dd



-- (Text, Int32, Int32, Int32, Int32, Maybe Text, Bool, Bool, Maybe Day, Int32)
{-
document (title, domain_fk, doc_type_fk, tier_fk, status_fk,
     owner_user_fk, residency, ai_allowed, legal_hold, due_date,
     created_by_user_fk)
  values
    ($1::text, $2::int4, $3::int4 ,$4::int4 ,$5::int4
    , $6::text?, $7::bool, $8::bool, $9::date?
    , $10::int4)
-}
createDoc :: Pool -> Int32 -> Text -> Int32 -> Int32 -> Int32 -> Int32 -> Maybe Int32
          -> Maybe Text -> Bool -> Bool -> Maybe Day
          -> IO (DataResult Int32)
createDoc pool actor title domainID typeID tierID statusID ownerID residency aiAllowed legalHold due =
  use pool (statement (title, domainID, typeID, tierID, statusID, ownerID
            , residency, aiAllowed, legalHold, due, actor
          ) St.qCreateDoc)
    <&> either (Left . show) Right


updateDocMeta :: Pool -> Text -> Int32 -> Int32 -> Int32 -> Int32 -> Maybe Int32 -> Maybe Text -> Bool -> Bool -> Maybe Day -> Int32 -> IO (DataResult ())
updateDocMeta pool title dom typ tier status owner residency aiAllowed legalHold due docId =
  use pool (statement (title, dom, typ, tier, status, owner
        , residency, aiAllowed, legalHold, due, docId
      ) St.qUpdateDocMeta)
    <&> either (Left . show) Right


softDeleteDoc :: Pool -> Int32 -> IO (DataResult ())
softDeleteDoc pool docId =
  use pool (statement docId St.qSoftDeleteDoc) <&> either (Left . show) Right

-- Versions ---------------------------------------------------
saveVersion :: Pool -> Int32 -> Int32 -> Maybe Text -> Maybe Text -> Int32 -> IO (DataResult Int32)
saveVersion pool docId verNo note contentRef author =
  use pool (statement (docId, verNo, note, contentRef, author) St.qInsertVersion) <&> either (Left . show) Right


latestVersion :: Pool -> Int32 -> IO (DataResult (Maybe DocVersion))
latestVersion pool docId =
  use pool (statement docId St.qLatestVersion) <&> either (Left . show) Right


-- Comments ---------------------------------------------------
listCommentsIO :: Pool -> Int32 -> IO (DataResult (V.Vector Comment))
listCommentsIO pool docId =
  use pool (statement docId St.qListComments) <&> either (Left . show) Right

addCommentIO :: Pool -> Int32 -> Int32 -> Maybe Int32 -> Text -> IO (DataResult Int32)
addCommentIO pool actor docId parent body =
  use pool (statement (docId, parent, actor, body) St.qAddComment) <&> either (Left . show) Right

deleteCommentIO :: Pool -> Int32 -> Int32 -> IO (DataResult ())
deleteCommentIO pool actor commentId =
  use pool (statement (commentId, actor) St.qDeleteComment) <&> either (Left . show) Right


-- ACLs -------------------------------------------------------
listAclIO :: Pool -> Int32 -> IO (DataResult (V.Vector AclEntry))
listAclIO pool docId =
  use pool (statement docId St.qListAcl) <&> either (Left . show) Right


addAclIO :: Pool -> Int32 -> Int32 -> Text -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> V.Vector Text -> Maybe Text -> Maybe Text -> IO (DataResult Int32)
addAclIO pool actor docId principal u g r o rights scope scopeVal =
  use pool (statement (docId, principal, u, g, r, o, rights, scope
            , scopeVal, actor
        ) St.qAddAcl)
    <&> either (Left . show) Right


removeAclIO :: Pool -> Int32 -> IO (DataResult ())
removeAclIO pool aclId =
  use pool (statement aclId St.qRemoveAcl) <&> either (Left . show) Right


-- Reports ----------------------------------------------------
heatmapCountsIO :: Pool -> IO (DataResult (V.Vector CountCell))
heatmapCountsIO pool =
  use pool (statement () St.qHeatmapCounts) <&> either (Left . show) Right


sankeyDomainStatusIO :: Pool -> IO (DataResult (V.Vector SankeyAB))
sankeyDomainStatusIO pool =
  use pool (statement () St.qSankeyDomainStatus) <&> either (Left . show) Right


sankeyStatusTierIO :: Pool -> IO (DataResult (V.Vector SankeyAB))
sankeyStatusTierIO pool =
  use pool (statement () St.qSankeyStatusTier) <&> either (Left . show) Right


-- Audit ------------------------------------------------------
recordAuditIO :: Pool -> Int32 -> Text -> Maybe Int32 -> Maybe Text -> Maybe Int32 -> Maybe Text -> IO (DataResult ())
recordAuditIO pool actor action mDoc mTargetType mTargetUid mUA =
  use pool (statement (actor, action, mDoc, mTargetType, mTargetUid, mUA) St.qAudit) <&> either (Left . show) Right
