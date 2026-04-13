{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Core.Attributes where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import HBDoc.Core.BlockKind (NoteKind)

data StyleAttrs = StyleAttrs {
    nameSA :: !(Maybe Text)
  , idSA :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data NumberingAttrs = NumberingAttrs {
    numIdNA :: !(Maybe Int32)
  , ilvlNA :: !(Maybe Int32)
  , numFmtNA :: !(Maybe Text)
  , lvlTextNA :: !(Maybe Text)
  , startAtNA :: !(Maybe Int32)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data MediaAttrs = MediaAttrs {
    imageRidMa :: !(Maybe Text)
  , imageNameMa :: !(Maybe Text)
  , imageKeyMa :: !(Maybe Text)
  , widthEmuMa :: !(Maybe Int32)
  , heightEmuMa :: !(Maybe Int32)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data NoteRefAttrs = NoteRefAttrs {
    noteIdNra :: !(Maybe Int32)
  , noteKindNra :: !(Maybe NoteKind)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data HtmlAttrs = HtmlAttrs {
    htmlIdHa :: !(Maybe Text)
  , htmlClassesHa :: ![Text]
  , htmlAttrsHa :: !(Map Text Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Fidelity / rendering / source-origin attrs.
-- Not the place for semantic list logic.
data AttributesBlk = AttributesBlk {
    captionAb :: !(Maybe Text)
  , styleAb :: !(Maybe StyleAttrs)
  , numberingAb :: !(Maybe NumberingAttrs)
  , mediaAb :: !(Maybe MediaAttrs)
  , noteRefAb :: !(Maybe NoteRefAttrs)
  , htmlAb :: !(Maybe HtmlAttrs)
  , customAb :: !(Map Text Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

emptyAttribs :: AttributesBlk
emptyAttribs = AttributesBlk {
    captionAb = Nothing
    , styleAb = Nothing
    , numberingAb = Nothing
    , mediaAb = Nothing
    , noteRefAb = Nothing
    , htmlAb = Nothing
    , customAb = mempty
    }
