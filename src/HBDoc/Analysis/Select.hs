module HBDoc.Analysis.Select (
  BlockPred,
  FeaturePred,
  andBlockPred,
  orBlockPred,
  notBlockPred,
  byKindPred,
  underPathPred,
  withTextPred,
  insideKindPred,
  hasInferencePred,
  definitionListPred,
  parsedMarkerListItemPred,
  selectBlocks,
  selectSubtree,
  selectDescendantsMatching,
  selectByKind,
  selectWithText,
  selectHeadings,
  selectParagraphs,
  selectLists,
  selectDefinitionLists,
  selectListItems,
  selectListItemsWithParsedMarkers,
  selectMessages,
  selectWithInference,
  selectInsideKind,
  selectUnderHeading,
  selectFeatures,
  selectFeatureByKind,
  groupFeaturesByHeading,
  selectTexts,
  selectFeatureRows
) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import HBDoc.Analysis.Context (insideKindCtx)
import HBDoc.Analysis.Features (BlockFeatures(..), extractBlockFeatures)
import HBDoc.Analysis.Flatten (FlattenCfg, flattenBlockText)
import HBDoc.Analysis.Path (isAncestorPathOf, rootPath)
import HBDoc.Analysis.Traverse (walkBlocksPreorder)
import HBDoc.Analysis.Types (BlockCtx(..), PathBlk)
import HBDoc.Core.BlockKind (KindBlk(..))
import HBDoc.Core.ListSemantics (ListKind(..), MarkerSource(..), levelHdg, kindLs, markerSourceLsi)
import HBDoc.Core.Provenance (inferencePB)
import HBDoc.Core.Semantics (SemanticsBlk(..))
import HBDoc.Core.Types (Block(..), HBDoc)

type BlockPred docSpec blkSpec = BlockCtx docSpec blkSpec -> Bool
type FeaturePred = BlockFeatures -> Bool

andBlockPred :: BlockPred d b -> BlockPred d b -> BlockPred d b
andBlockPred predA predB ctx = predA ctx && predB ctx

orBlockPred :: BlockPred d b -> BlockPred d b -> BlockPred d b
orBlockPred predA predB ctx = predA ctx || predB ctx

notBlockPred :: BlockPred d b -> BlockPred d b
notBlockPred predA ctx = not (predA ctx)

byKindPred :: KindBlk -> BlockPred d b
byKindPred kind ctx = ctx.blockBc.kindBk == kind

underPathPred :: PathBlk -> BlockPred d b
underPathPred path ctx =
  pathBc ctx == path || isAncestorPathOf path (pathBc ctx)

withTextPred :: BlockPred d b
withTextPred ctx =
  hasLocalTextBk (blockBc ctx)

insideKindPred :: KindBlk -> BlockPred d b
insideKindPred = insideKindCtx

hasInferencePred :: BlockPred d b
hasInferencePred ctx =
  case provenanceBk (blockBc ctx) of
    Nothing -> False
    Just provenance -> isJust (inferencePB provenance)

definitionListPred :: BlockPred d b
definitionListPred ctx =
  case semBk (blockBc ctx) of
    Just (ListSB sem) -> kindLs sem == DefinitionLK
    _ -> False

parsedMarkerListItemPred :: BlockPred d b
parsedMarkerListItemPred ctx =
  case semBk (blockBc ctx) of
    Just (ListItemSB sem) -> markerSourceLsi sem == ParsedMarkerMS
    _ -> False

selectBlocks :: BlockPred d b -> HBDoc d b -> [BlockCtx d b]
selectBlocks predA doc =
  filter predA (walkBlocksPreorder doc)

selectSubtree :: PathBlk -> HBDoc d b -> [BlockCtx d b]
selectSubtree path =
  selectBlocks (underPathPred path)

selectDescendantsMatching :: PathBlk -> BlockPred d b -> HBDoc d b -> [BlockCtx d b]
selectDescendantsMatching path predA doc =
  filter isMatch (walkBlocksPreorder doc)
  where
    isMatch ctx =
      pathBc ctx /= path
      && underPathPred path ctx
      && predA ctx

selectByKind :: KindBlk -> HBDoc d b -> [BlockCtx d b]
selectByKind kind =
  selectBlocks (byKindPred kind)

selectWithText :: HBDoc d b -> [BlockCtx d b]
selectWithText =
  selectBlocks withTextPred

selectHeadings :: HBDoc d b -> [BlockCtx d b]
selectHeadings =
  selectByKind HeadingKB

selectParagraphs :: HBDoc d b -> [BlockCtx d b]
selectParagraphs =
  selectByKind ParagraphKB

selectLists :: HBDoc d b -> [BlockCtx d b]
selectLists =
  selectByKind ListKB

selectDefinitionLists :: HBDoc d b -> [BlockCtx d b]
selectDefinitionLists =
  selectBlocks definitionListPred

selectListItems :: HBDoc d b -> [BlockCtx d b]
selectListItems =
  selectByKind ListItemKB

selectListItemsWithParsedMarkers :: HBDoc d b -> [BlockCtx d b]
selectListItemsWithParsedMarkers =
  selectBlocks parsedMarkerListItemPred

selectMessages :: HBDoc d b -> [BlockCtx d b]
selectMessages =
  selectByKind MessageKB

selectWithInference :: HBDoc d b -> [BlockCtx d b]
selectWithInference =
  selectBlocks hasInferencePred

selectInsideKind :: KindBlk -> HBDoc d b -> [BlockCtx d b]
selectInsideKind kind =
  selectBlocks (insideKindPred kind)

-- | Select the blocks governed by headings that satisfy the predicate.
-- The heading block itself is not returned; only the section content below it.
selectUnderHeading :: BlockPred d b -> HBDoc d b -> [BlockCtx d b]
selectUnderHeading headingPred doc =
  orderedDistinctCtxs selected
  where
    ctxs = walkBlocksPreorder doc
    headings = filter (\ctx -> isHeadingCtx ctx && headingPred ctx) ctxs
    selected = concatMap (\ctx -> sectionCtxsFromHeading ctx ctxs) headings

selectFeatures :: FeaturePred -> [BlockFeatures] -> [BlockFeatures]
selectFeatures =
  filter

selectFeatureByKind :: KindBlk -> [BlockFeatures] -> [BlockFeatures]
selectFeatureByKind kind =
  filter (\feat -> kindBf feat == kind)

groupFeaturesByHeading :: [BlockFeatures] -> Map PathBlk [BlockFeatures]
groupFeaturesByHeading =
  foldl' step M.empty
  where
    step acc feat =
      M.insertWith (\new old -> old <> new) (headingGroupKey feat) [feat] acc

    headingGroupKey feat =
      case kindBf feat of
        HeadingKB -> pathBf feat
        _ -> fromMaybe rootPath (nearestHeadingPathBf feat)

selectTexts :: BlockPred d b -> FlattenCfg -> HBDoc d b -> [Text]
selectTexts predA cfg doc =
  map (flattenBlockText cfg) (selectBlocks predA doc)

selectFeatureRows :: BlockPred d b -> FlattenCfg -> HBDoc d b -> [BlockFeatures]
selectFeatureRows predA cfg doc =
  map (extractBlockFeatures cfg doc) (selectBlocks predA doc)

isHeadingCtx :: BlockCtx d b -> Bool
isHeadingCtx ctx =
  kindBk (blockBc ctx) == HeadingKB

headingLevelCtx :: BlockCtx d b -> Maybe Int
headingLevelCtx ctx =
  case semBk (blockBc ctx) of
    Just (HeadingSB sem) -> Just (fromIntegral (levelHdg sem))
    _ -> Nothing

sectionCtxsFromHeading :: BlockCtx d b -> [BlockCtx d b] -> [BlockCtx d b]
sectionCtxsFromHeading headingCtx ctxs =
  case headingLevelCtx headingCtx of
    Nothing -> []
    Just levelH ->
      case dropWhile (\ctx -> pathBc ctx /= pathBc headingCtx) ctxs of
        [] -> []
        _ : rest -> takeWhile (not . isBoundary levelH) rest
  where
    headingPath = pathBc headingCtx

    isBoundary levelH ctx =
      case headingLevelCtx ctx of
        Nothing -> False
        Just levelOther ->
          let isSelfOrDesc =
                pathBc ctx == headingPath || isAncestorPathOf headingPath (pathBc ctx)
          in not isSelfOrDesc && levelOther <= levelH

orderedDistinctCtxs :: [BlockCtx d b] -> [BlockCtx d b]
orderedDistinctCtxs ctxs =
  reverse keptRev
  where
    (_, keptRev) = foldl' step (S.empty, []) ctxs

    step :: (Set PathBlk, [BlockCtx d b]) -> BlockCtx d b -> (Set PathBlk, [BlockCtx d b])
    step (seen, kept) ctx
      | S.member (pathBc ctx) seen = (seen, kept)
      | otherwise = (S.insert (pathBc ctx) seen, ctx : kept)

hasLocalTextBk :: Block b -> Bool
hasLocalTextBk blk =
  case contentBk blk of
    Nothing -> False
    Just txt -> not (T.null (T.strip txt))