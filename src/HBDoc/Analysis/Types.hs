{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Analysis.Types where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import HBDoc.Core.Attributes
import HBDoc.Core.BlockKind
import HBDoc.Core.ListSemantics
import HBDoc.Core.Provenance
import HBDoc.Core.Semantics
import HBDoc.Core.Types

-- | Stable child index under a parent block.
newtype IndexBlk = IndexBlk Int32
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Root-relative depth.
-- Root is depth 0.
newtype DepthBlk = DepthBlk Int32
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Pre-order traversal index.
newtype PreorderIx = PreorderIx Int32
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Root-relative canonical path.
-- The root block itself is 'PathBlk []'.
newtype PathBlk = PathBlk [IndexBlk]
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Half-open local text span [start, end).
data TextSpan = TextSpan {
    startTs :: !Int32
  , endTs :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Lightweight ancestor snapshot used by traversal and context derivation.
-- The ancestor list in 'BlockCtx' is ordered from root to direct parent.
data AncestorFrame blkSpec = AncestorFrame {
    pathAf :: !PathBlk
  , depthAf :: !DepthBlk
  , kindAf :: !KindBlk
  , contentAf :: !(Maybe Text)
  , semAf :: !(Maybe SemanticsBlk)
  , attribsAf :: !AttributesBlk
  , provenanceAf :: !(Maybe ProvenanceBlk)
  , childCountAf :: !Int32
  , extraAf :: blkSpec
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Canonical traversal context.
--
-- Invariants intended for traversal producers:
--
-- * 'ancestorsBc' is ordered from root to direct parent.
-- * nearest-path fields refer to the nearest enclosing block of that class,
--   including the current block when it matches that class.
-- * for the root block, 'parentPathBc' is Nothing, 'siblingIxBc' should be 0,
--   and 'siblingCountBc' should be 1.
data BlockCtx docSpec blkSpec = BlockCtx {
    docTitleBc :: !Text
  , docFormatBc :: !(Maybe Text)
  , docMetaBc :: !(Map Text MetaValue)
  , docExtraBc :: docSpec
  , blockBc :: !(Block blkSpec)
  , pathBc :: !PathBlk
  , parentPathBc :: !(Maybe PathBlk)
  , depthBc :: !DepthBlk
  , preorderIxBc :: !PreorderIx
  , siblingIxBc :: !IndexBlk
  , siblingCountBc :: !Int32
  , ancestorsBc :: ![AncestorFrame blkSpec]
  , headingPathBc :: !(Maybe PathBlk)
  , listPathBc :: !(Maybe PathBlk)
  , listItemPathBc :: !(Maybe PathBlk)
  , tablePathBc :: !(Maybe PathBlk)
  , notePathBc :: !(Maybe PathBlk)
  , conversationPathBc :: !(Maybe PathBlk)
  , customPathBc :: !(Maybe PathBlk)
  , customTagBc :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Canonical flattening view.
data FlattenMode =
    LocalVisibleFM
  | SubtreeVisibleFM
  | SectionFM
  | HeadingPathFM
  | ListItemFM
  | MessageFM
  | TableProjectionFM
  | DocumentFM
  | CustomFM Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Canonical chunk categories emitted by flattening.
data ChunkKind =
    HeadingTextCK
  | ParagraphTextCK
  | ListMarkerCK
  | ListItemBodyCK
  | QuoteTextCK
  | CodeTextCK
  | TableCaptionCK
  | TableCellCK
  | FigureCaptionCK
  | ImageAltCK
  | NoteTextCK
  | MessageRoleCK
  | MessageAuthorCK
  | MessageTsCK
  | MessageBodyCK
  | SyntheticSeparatorCK
  | CustomTextCK Text
  | UnknownCK
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | One flattened text chunk with source identity.
data FlattenChunk = FlattenChunk {
    pathFch :: !PathBlk
  , kindFch :: !ChunkKind
  , textFch :: !Text
  , spanFch :: !(Maybe TextSpan)
  , syntheticFch :: !Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Normalized marker class used by the canonical feature row.
data MarkerClass =
    LabelledMC
  | GraphicMC
  | NativeOrderedMC
  | NativeBulletMC
  | NativeDefinitionMC
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


data KindCount = KindCount {
    kindKc :: !KindBlk
  , countKc :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data HeadingLevelCount = HeadingLevelCount {
    levelHlc :: !Int32
  , countHlc :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ListKindCount = ListKindCount {
    kindLkc :: !ListKind
  , countLkc :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SourceCount = SourceCount {
    sourceSc :: !StructureSource
  , countSc :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data InferenceCount = InferenceCount {
    inferenceIc :: !InferenceKind
  , countIc :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data CustomTagCount = CustomTagCount {
    tagCtc :: !Text
  , countCtc :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Canonical document-level summary record.

type BlockPred docSpec blkSpec = BlockCtx docSpec blkSpec -> Bool
-- type FeaturePred = BlockFeatures -> Bool

mkIndexBlk :: Int32 -> IndexBlk
mkIndexBlk = IndexBlk

unIndexBlk :: IndexBlk -> Int32
unIndexBlk (IndexBlk n) = n

mkDepthBlk :: Int32 -> DepthBlk
mkDepthBlk = DepthBlk

unDepthBlk :: DepthBlk -> Int32
unDepthBlk (DepthBlk n) = n

mkPreorderIx :: Int32 -> PreorderIx
mkPreorderIx = PreorderIx

unPreorderIx :: PreorderIx -> Int32
unPreorderIx (PreorderIx n) = n

mkPathBlk :: [IndexBlk] -> PathBlk
mkPathBlk = PathBlk

pathFromInt32sBlk :: [Int32] -> PathBlk
pathFromInt32sBlk =
  PathBlk . map IndexBlk

unPathBlk :: PathBlk -> [IndexBlk]
unPathBlk (PathBlk xs) = xs

pathToInt32sBlk :: PathBlk -> [Int32]
pathToInt32sBlk (PathBlk xs) =
  map unIndexBlk xs

rootPathBlk :: PathBlk
rootPathBlk = PathBlk []

isRootPathBlk :: PathBlk -> Bool
isRootPathBlk (PathBlk xs) =
  null xs

renderPathBlk :: PathBlk -> Text
renderPathBlk (PathBlk xs) =
  case xs of
    [] -> "root"
    _ -> "root." <> T.intercalate "." (map (T.pack . show . unIndexBlk) xs)

markerClassFromMarker :: ListMarker -> MarkerClass
markerClassFromMarker marker =
  case marker of
    MarkerLabelled _ -> LabelledMC
    MarkerGraphic _ -> GraphicMC
    MarkerNativeOrdered _ -> NativeOrderedMC
    MarkerNativeBullet -> NativeBulletMC
    MarkerNativeDefinition -> NativeDefinitionMC

presentationHeadingLevel :: ItemPresentation -> Int32
presentationHeadingLevel presentation =
  case presentation of
    HeadingPresentationIP lvl -> lvl
