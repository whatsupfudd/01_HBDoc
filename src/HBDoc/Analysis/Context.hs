{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{- HLINT ignore "Use =<<" -}

module HBDoc.Analysis.Context (
    HeadingContext(..)
  , headingContextCtx
  , nearestHeadingCtx
  , nearestHeadingAncestorCtx
  , headingChainCtx
  , headingLineageText
  , nearestListCtx
  , nearestListAncestorCtx
  , nearestDefinitionListCtx
  , insideDefinitionListCtx
  , nearestListItemCtx
  , nearestListItemAncestorCtx
  , listDepthCtx
  , enclosingSectionRootCtx
  , enclosingCustomTagCtx
  , insideKindCtx
  , insideCustomTagCtx
  , previousSiblingCtx
  , nextSiblingCtx
  , previousSiblingOfKindCtx
  , nextSiblingOfKindCtx
  , firstHeadingDescendantCtx
  , precedingHeadingCtx
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.List (find, foldl', sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import HBDoc.Analysis.Flatten (FlattenCfg, flattenBlockText)
import HBDoc.Analysis.Path (appendChildPath, isAncestorPathOf, parentPath, pathToList)
import HBDoc.Analysis.Traverse (lookupCtxByPath, walkBlocksPreorder)
import HBDoc.Analysis.Types (BlockCtx(..), IndexBlk(..), PathBlk)
import HBDoc.Core.BlockKind (KindBlk(..))
import HBDoc.Core.ListSemantics (
    HeadingSemantics,
    HeadingSource,
    ListKind(..),
    ListLabel,
    kindLs,
    labelHdg,
    levelHdg,
    sourceHdg
  )
import HBDoc.Core.Semantics (SemanticsBlk(..))
import HBDoc.Core.Types (Block(..), HBDoc)

data HeadingContext = HeadingContext {
    pathHcx :: !PathBlk
  , textHcx :: !(Maybe Text)
  , levelHcx :: !Int32
  , labelHcx :: !(Maybe ListLabel)
  , sourceHcx :: !HeadingSource
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

headingContextCtx :: BlockCtx d b -> Maybe HeadingContext
headingContextCtx ctx =
  case headingSemCtx ctx of
    Nothing -> Nothing
    Just sem ->
      let
        blk = ctx.blockBc
      in
      Just HeadingContext {
          pathHcx = ctx.pathBc
        , textHcx = blk.contentBk
        , levelHcx = levelHdg sem
        , labelHcx = labelHdg sem
        , sourceHcx = sourceHdg sem
        }


nearestHeadingCtx :: BlockCtx d b -> Maybe (BlockCtx d b)
nearestHeadingCtx =
  nearestCurrentOrAncestorCtx isHeadingCtx

nearestHeadingAncestorCtx :: BlockCtx d b -> Maybe (BlockCtx d b)
nearestHeadingAncestorCtx =
  nearestAncestorCtx isHeadingCtx

headingChainCtx :: BlockCtx d b -> [BlockCtx d b]
headingChainCtx ctx =
  let ancestorHeadings = filter isHeadingCtx (ancestorChainCtx ctx)
      selfHeading = if isHeadingCtx ctx then [ctx] else []
  in ancestorHeadings <> selfHeading

headingLineageText :: FlattenCfg -> BlockCtx d b -> Text
headingLineageText cfg ctx =
  let parts =
        [ T.strip (flattenBlockText cfg headingCtx)
        | headingCtx <- headingChainCtx ctx
        ]
  in T.intercalate " > " (filter (not . T.null) parts)

nearestListCtx :: BlockCtx d b -> Maybe (BlockCtx d b)
nearestListCtx =
  nearestCurrentOrAncestorCtx isListCtx

nearestListAncestorCtx :: BlockCtx d b -> Maybe (BlockCtx d b)
nearestListAncestorCtx =
  nearestAncestorCtx isListCtx

nearestDefinitionListCtx :: BlockCtx d b -> Maybe (BlockCtx d b)
nearestDefinitionListCtx =
  nearestCurrentOrAncestorCtx isDefinitionListCtx

insideDefinitionListCtx :: BlockCtx d b -> Bool
insideDefinitionListCtx ctx =
  case nearestDefinitionListCtx ctx of
    Nothing -> False
    Just _ -> True

nearestListItemCtx :: BlockCtx d b -> Maybe (BlockCtx d b)
nearestListItemCtx =
  nearestCurrentOrAncestorCtx isListItemCtx

nearestListItemAncestorCtx :: BlockCtx d b -> Maybe (BlockCtx d b)
nearestListItemAncestorCtx =
  nearestAncestorCtx isListItemCtx

listDepthCtx :: BlockCtx d b -> Int32
listDepthCtx ctx =
  fromIntegral (length (filter isListCtx (ctx : ancestorChainCtx ctx)))

enclosingSectionRootCtx :: HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
enclosingSectionRootCtx doc ctx
  | isHeadingCtx ctx = Just ctx
  | otherwise =
      case nearestHeadingAncestorCtx ctx of
        Just headingCtx -> Just headingCtx
        Nothing -> precedingHeadingCtx doc ctx

enclosingCustomTagCtx :: Text -> BlockCtx d b -> Maybe (BlockCtx d b)
enclosingCustomTagCtx tag =
  nearestCurrentOrAncestorCtx (hasCustomTagCtx tag)

insideKindCtx :: KindBlk -> BlockCtx d b -> Bool
insideKindCtx kind ctx =
  any (isKindCtx kind) (ctx : ancestorChainCtx ctx)

insideCustomTagCtx :: Text -> BlockCtx d b -> Bool
insideCustomTagCtx tag ctx =
  case enclosingCustomTagCtx tag ctx of
    Nothing -> False
    Just _ -> True

previousSiblingCtx :: HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
previousSiblingCtx =
  siblingRelativeCtx (-1)

nextSiblingCtx :: HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
nextSiblingCtx =
  siblingRelativeCtx 1

previousSiblingOfKindCtx :: KindBlk -> HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
previousSiblingOfKindCtx kind =
  siblingSeekCtx (-1) (isKindCtx kind)

nextSiblingOfKindCtx :: KindBlk -> HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
nextSiblingOfKindCtx kind =
  siblingSeekCtx 1 (isKindCtx kind)

firstHeadingDescendantCtx :: HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
firstHeadingDescendantCtx doc ctx =
  let
    selfPath = ctx.pathBc
  in
  find (\cand ->
        isHeadingCtx cand
        && cand.pathBc /= selfPath
        && isAncestorPathOf selfPath cand.pathBc
    ) (walkBlocksPreorder doc)

precedingHeadingCtx :: HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
precedingHeadingCtx doc ctx =
  foldl' step Nothing (walkBlocksPreorder doc)
  where
    step acc cand
      | cand.preorderIxBc < ctx.preorderIxBc && isHeadingCtx cand = Just cand
      | otherwise = acc


ancestorChainCtx :: BlockCtx d b -> [BlockCtx d b]
ancestorChainCtx ctx =
  -- TODO: find out how to recover a list of ancestor BlockCtx from the ancestorsBc field
  -- sortOn (length . pathToList . pathBc) ctx.ancestorsBc
  []

nearestAncestorCtx :: (BlockCtx d b -> Bool) -> BlockCtx d b -> Maybe (BlockCtx d b)
nearestAncestorCtx predCtx ctx =
  case filter predCtx (ancestorChainCtx ctx) of
    [] -> Nothing
    matches -> Just (last matches)

nearestCurrentOrAncestorCtx :: (BlockCtx d b -> Bool) -> BlockCtx d b -> Maybe (BlockCtx d b)
nearestCurrentOrAncestorCtx predCtx ctx
  | predCtx ctx = Just ctx
  | otherwise = nearestAncestorCtx predCtx ctx

siblingRelativeCtx :: Int32 -> HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
siblingRelativeCtx step doc ctx = do
  targetPath <- shiftSiblingPath step ctx.pathBc
  lookupCtxByPath targetPath doc

siblingSeekCtx :: Int32 -> (BlockCtx d b -> Bool) -> HBDoc d b -> BlockCtx d b -> Maybe (BlockCtx d b)
siblingSeekCtx step predCtx doc ctx = do
  startPath <- shiftSiblingPath step ctx.pathBc
  iterPath startPath
  where
  iterPath path =
    case lookupCtxByPath path doc of
      Nothing -> Nothing
      Just siblingCtx
        | predCtx siblingCtx -> Just siblingCtx
        | otherwise -> maybe Nothing iterPath (shiftSiblingPath step path)


shiftSiblingPath :: Int32 -> PathBlk -> Maybe PathBlk
shiftSiblingPath step path = do
  parPath <- parentPath path
  IndexBlk idx <- lastIndexPath path
  let idx' = idx + step
  if idx' < 0
    then Nothing
    else Just (appendChildPath parPath (IndexBlk idx'))

lastIndexPath :: PathBlk -> Maybe IndexBlk
lastIndexPath path =
  case reverse (pathToList path) of
    [] -> Nothing
    idx : _ -> Just idx

isKindCtx :: KindBlk -> BlockCtx d b -> Bool
isKindCtx kind ctx = ctx.blockBc.kindBk == kind

hasCustomTagCtx :: Text -> BlockCtx d b -> Bool
hasCustomTagCtx tag ctx = case ctx.blockBc.kindBk of
  CustomKB tag' -> tag' == tag
  _ -> False


isHeadingCtx :: BlockCtx d b -> Bool
isHeadingCtx ctx =
  let
    blk = ctx.blockBc
  in
  case (kindBk blk, semBk blk) of
    (HeadingKB, Just (HeadingSB _)) -> True
    _ -> False


isListCtx :: BlockCtx d b -> Bool
isListCtx ctx =
  let
    blk = ctx.blockBc
  in
  case (kindBk blk, semBk blk) of
    (ListKB, Just (ListSB _)) -> True
    _ -> False


isDefinitionListCtx :: BlockCtx d b -> Bool
isDefinitionListCtx ctx =
  let
    blk = ctx.blockBc
  in
  case (blk.kindBk, blk.semBk) of
    (ListKB, Just (ListSB sem)) -> sem.kindLs == DefinitionLK
    _ -> False


isListItemCtx :: BlockCtx d b -> Bool
isListItemCtx ctx =
  let
    blk = ctx.blockBc
  in
  case (blk.kindBk, blk.semBk) of
    (ListItemKB, Just (ListItemSB _)) -> True
    _ -> False


headingSemCtx :: BlockCtx d b -> Maybe HeadingSemantics
headingSemCtx ctx =
  case (kindBk blk, semBk blk) of
    (HeadingKB, Just (HeadingSB sem)) -> Just sem
    _ -> Nothing
  where
    blk = ctx.blockBc