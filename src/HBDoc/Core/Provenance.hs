{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Core.Provenance where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data StructureSource =
    PandocSS
  | DocxXmlSS
  | MarkdownSS
  | NotionSS
  | ConversationSS
  | StructDocSS
  | ManualEditSS
  | OtherSS Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data InferenceKind =
    ListReconstructionIK
  | HeadingPromotionIK
  | HeadingLabelParseIK
  | ListContinuationIK
  | StructureNormalizationIK
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ProvenanceBlk = ProvenanceBlk {
    sourcePB :: !StructureSource
  , sourceRefPB :: !(Maybe Text)
  , inferencePB :: !(Maybe InferenceKind)
  , confidencePB :: !(Maybe Double)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)