{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Core.Types where

import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import HBDoc.Core.Attributes
import HBDoc.Core.BlockKind
import HBDoc.Core.Provenance
import HBDoc.Core.Semantics
import Data.Void (Void)

data MetaValue =
    MetaTextMV Text
  | MetaIntMV Int64
  | MetaBoolMV Bool
  | MetaListMV [MetaValue]
  | MetaMapMV (Map Text MetaValue)
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Canonical document root.
--
-- rootBlk is always a ContainerBK block.

data HBDoc docSpec blkSpec = HBDoc {
  titleDc :: Text,
  formatDc :: Maybe Text,
  metaDefDc :: Map Text MetaValue,
  rootBlkDc :: Block blkSpec,
  extraDc :: docSpec
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- | Canonical block node for all ingestion and editing.
--
-- Invariant:
--  the spec type must match the one used in the HBDoc constructor.
--   kindBlk and semBlk must agree.
--
-- Examples:
--   HeadingBK   <-> Just (HeadingSemBS ...)
--   ListBK      <-> Just (ListSemBS ...)
--   ListItemBK  <-> Just (ListItemSemBS ...)
--   ParagraphBK <-> Nothing

data Block spec = Block {
  kindBk :: KindBlk
  , contentBk :: Maybe Text
  , semBk :: Maybe SemanticsBlk
  , attribsBk :: AttributesBlk
  , provenanceBk :: Maybe ProvenanceBlk
  , childrenBk :: [Block spec]
  , extraBk :: spec
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


data ExtraInfoBlk =
  EmptyEI
  | DbRefEI !Int32 !Text
  | TrackedEI !Int32 !Text !Text
