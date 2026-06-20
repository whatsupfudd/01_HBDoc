module HBDoc.Analysis.Path
  ( IndexBlk(..)
  , DepthBlk(..)
  , PathBlk(..)
  , rootPath
  , listToPath
  , pathToList
  , renderPath
  , appendChildPath
  , parentPath
  , ancestorPaths
  , pathDepth
  , isRootPath
  , siblingIndexPath
  , comparePath
  , sortPaths
  , isPathPrefixOf
  , isStrictPrefixPathOf
  , isAncestorPathOf
  , isDescendantPathOf
  , commonPrefixPath
  , lookupBlockAtPath
  , pathExistsInDoc
  ) where

import Data.List (inits, sortBy)
import Data.Text (Text)
import qualified Data.Text as T

import HBDoc.Analysis.Types (DepthBlk(..), IndexBlk(..), PathBlk(..))
import HBDoc.Core.Types (Block(..), HBDoc(..))

rootPath :: PathBlk
rootPath =
  PathBlk []

listToPath :: [IndexBlk] -> PathBlk
listToPath steps =
  PathBlk steps

pathToList :: PathBlk -> [IndexBlk]
pathToList path =
  case path of
    PathBlk steps -> steps

renderPath :: PathBlk -> Text
renderPath path =
  case pathToList path of
    [] -> "root"
    steps -> "root" <> T.concat (map renderStep steps)
  where
    renderStep :: IndexBlk -> Text
    renderStep step =
      case step of
        IndexBlk ix -> "." <> T.pack (show ix)

appendChildPath :: PathBlk -> IndexBlk -> PathBlk
appendChildPath path childIx =
  PathBlk (pathToList path <> [childIx])

parentPath :: PathBlk -> Maybe PathBlk
parentPath path =
  case unsnocList (pathToList path) of
    Nothing -> Nothing
    Just (parentSteps, _) -> Just (PathBlk parentSteps)

ancestorPaths :: PathBlk -> [PathBlk]
ancestorPaths path =
  let steps = pathToList path
  in map PathBlk (take (length steps) (inits steps))

pathDepth :: PathBlk -> DepthBlk
pathDepth path =
  DepthBlk (fromIntegral (length (pathToList path)))

isRootPath :: PathBlk -> Bool
isRootPath path =
  null (pathToList path)

siblingIndexPath :: PathBlk -> Maybe IndexBlk
siblingIndexPath path =
  case unsnocList (pathToList path) of
    Nothing -> Nothing
    Just (_, childIx) -> Just childIx

comparePath :: PathBlk -> PathBlk -> Ordering
comparePath left right =
  compare (map indexValue (pathToList left)) (map indexValue (pathToList right))

sortPaths :: [PathBlk] -> [PathBlk]
sortPaths =
  sortBy comparePath

isPathPrefixOf :: PathBlk -> PathBlk -> Bool
isPathPrefixOf prefix full =
  let prefixSteps = pathToList prefix
      fullSteps = pathToList full
  in prefixSteps == take (length prefixSteps) fullSteps

isStrictPrefixPathOf :: PathBlk -> PathBlk -> Bool
isStrictPrefixPathOf prefix full =
  prefix /= full && isPathPrefixOf prefix full

isAncestorPathOf :: PathBlk -> PathBlk -> Bool
isAncestorPathOf ancestor descendant =
  isStrictPrefixPathOf ancestor descendant

isDescendantPathOf :: PathBlk -> PathBlk -> Bool
isDescendantPathOf descendant ancestor =
  isAncestorPathOf ancestor descendant

commonPrefixPath :: PathBlk -> PathBlk -> PathBlk
commonPrefixPath left right =
  PathBlk (go (pathToList left) (pathToList right))
  where
    go :: [IndexBlk] -> [IndexBlk] -> [IndexBlk]
    go leftSteps rightSteps =
      case (leftSteps, rightSteps) of
        (leftIx : leftRest, rightIx : rightRest)
          | leftIx == rightIx -> leftIx : go leftRest rightRest
        _ -> []

lookupBlockAtPath :: PathBlk -> HBDoc docSpec blkSpec -> Maybe (Block blkSpec)
lookupBlockAtPath path doc =
  go (pathToList path) (rootBlkDc doc)
  where
    go :: [IndexBlk] -> Block blkSpec -> Maybe (Block blkSpec)
    go steps blk =
      case steps of
        [] -> Just blk
        childIx : rest -> do
          childPos <- indexToInt childIx
          childBlk <- lookupAt childPos (childrenBk blk)
          go rest childBlk

pathExistsInDoc :: PathBlk -> HBDoc docSpec blkSpec -> Bool
pathExistsInDoc path doc =
  case lookupBlockAtPath path doc of
    Nothing -> False
    Just _ -> True

indexValue :: IndexBlk -> Int
indexValue idx =
  case idx of
    IndexBlk ix -> fromIntegral ix

indexToInt :: IndexBlk -> Maybe Int
indexToInt idx =
  case idx of
    IndexBlk ix
      | ix < 0 -> Nothing
      | otherwise -> Just (fromIntegral ix)

lookupAt :: Int -> [a] -> Maybe a
lookupAt n xs
  | n < 0 = Nothing
  | otherwise = go n xs
  where
    go :: Int -> [a] -> Maybe a
    go i ys =
      case (i, ys) of
        (_, []) -> Nothing
        (0, z : _) -> Just z
        (_, _ : zs) -> go (i - 1) zs

unsnocList :: [a] -> Maybe ([a], a)
unsnocList xs =
  case xs of
    [] -> Nothing
    y : ys -> Just (go [] y ys)
  where
    go :: [a] -> a -> [a] -> ([a], a)
    go acc lastSeen rest =
      case rest of
        [] -> (reverse acc, lastSeen)
        z : zs -> go (lastSeen : acc) z zs