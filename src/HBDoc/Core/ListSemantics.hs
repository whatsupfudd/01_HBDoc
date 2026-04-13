{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Core.ListSemantics where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data ListKind =
    OrderedLK
  | BulletLK
  | OutlineLK
  | DefinitionLK
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Where the list structure came from.
data ListSource =
    NativeListLS          -- source format had an explicit list
  | ParsedTextListLS      -- reconstructed from paragraph text
  | ParsedHeadingListLS   -- reconstructed from heading-like text
  | MixedListLS           -- partially native, partially inferred
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | How a specific item marker was obtained.
data MarkerSource =
    NativeMarkerMS
  | ParsedMarkerMS
  | GeneratedMarkerMS
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Display / structural interpretation of a list item.
newtype ItemPresentation = HeadingPresentationIP Int32
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data GraphicBullet =
    HyphenBulletGB
  | AsteriskBulletGB
  | SolidBulletGB
  | WhiteBulletGB
  | SquareBulletGB
  | TriangularBulletGB
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data LabelAtom =
    DecimalAtomLA Int32
  | UpperAlphaAtomLA Char
  | LowerAlphaAtomLA Char
  | UpperRomanAtomLA Int32
  | LowerRomanAtomLA Int32
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data LabelTrailer =
    LabelDotLT
  | LabelDashLT
  | LabelParenLT
  | LabelColonLT
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ListLabel = ListLabel {
    atomsLbl :: !(NonEmpty LabelAtom)
  , trailerLbl :: !(Maybe LabelTrailer)
  , rawLbl :: !Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ListMarker =
    MarkerLabelled ListLabel
  | MarkerGraphic GraphicBullet
  | MarkerNativeOrdered Int32
  | MarkerNativeBullet
  | MarkerNativeDefinition
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Semantics for a list container.
data ListSemantics = ListSemantics {
    kindLs :: !ListKind
  , sourceLs :: !ListSource
  , startAtLs :: !(Maybe Int32)
  , isTightLs :: !(Maybe Bool)
  , continuesPrevLs :: !(Maybe Bool)
  , seriesIdLs :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Semantics for one list item.
--
-- Nesting depth is derived from the tree, not duplicated here.
data ListItemSemantics = ListItemSemantics {
    markerLsi :: !(Maybe ListMarker)
  , markerSourceLsi :: !MarkerSource
  , presentationLsi :: !(Maybe ItemPresentation)
  , rawMarkerLsi :: !(Maybe Text)
  , isInferredLsi :: !Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data HeadingSource =
    NativeHeadingHS
  | PromotedListItemHS
  | ParsedLabelHeadingHS
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Headings can carry an optional outline/list label such as:
--   "IV.", "4.2", "(b)", etc.
data HeadingSemantics = HeadingSemantics {
    levelHdg :: !Int32
  , labelHdg :: !(Maybe ListLabel)
  , sourceHdg :: !HeadingSource
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)