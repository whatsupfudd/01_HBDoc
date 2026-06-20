{-# LANGUAGE RankNTypes #-}
module HBDoc.Analysis.Traverse where

import Data.Int (Int32)
import Data.List (foldl')
import Data.Monoid (Endo(..), appEndo)

import HBDoc.Analysis.Path
import HBDoc.Analysis.Types
import HBDoc.Core.Types

-- | Canonical pre-order traversal over the full document tree.
-- The root container is included as the first node.
walkBlocksPreorder :: HBDoc d b -> [BlockCtx d b]
walkBlocksPreorder doc =
  collectFromCtxPreorder (mkRootCtx doc)

-- | Canonical post-order traversal over the full document tree.
-- The root container is included as the last node.
walkBlocksPostorder :: HBDoc d b -> [BlockCtx d b]
walkBlocksPostorder doc =
  collectFromCtxPostorder (mkRootCtx doc)

-- | Canonical pre-order traversal over the subtree rooted at the given path.
-- Paths are document-global and the resulting contexts preserve document-global
-- path and pre-order indexing.
walkSubtreePreorder :: PathBlk -> HBDoc d b -> [BlockCtx d b]
walkSubtreePreorder path doc =
  case lookupCtxByPath path doc of
    Nothing -> []
    Just ctx -> collectFromCtxPreorder ctx

-- | Canonical post-order traversal over the subtree rooted at the given path.
-- Paths are document-global and the resulting contexts preserve document-global
-- path and pre-order indexing.
walkSubtreePostorder :: PathBlk -> HBDoc d b -> [BlockCtx d b]
walkSubtreePostorder path doc =
  case lookupCtxByPath path doc of
    Nothing -> []
    Just ctx -> collectFromCtxPostorder ctx

-- | Fold blocks in canonical pre-order.
foldBlocksPreorder :: (acc -> BlockCtx d b -> acc) -> acc -> HBDoc d b -> acc
foldBlocksPreorder step acc0 doc =
  snd (foldFromCtxPreorder step acc0 (mkRootCtx doc))

-- | Fold blocks in canonical post-order.
foldBlocksPostorder :: (acc -> BlockCtx d b -> acc) -> acc -> HBDoc d b -> acc
foldBlocksPostorder step acc0 doc =
  snd (foldFromCtxPostorder step acc0 (mkRootCtx doc))

-- | Map over blocks while preserving the original tree shape.
-- If the mapper returns a block with its own children, those children are
-- discarded and replaced by the recursively mapped original children.
mapBlocks :: (Block b -> Block b) -> HBDoc d b -> HBDoc d b
mapBlocks f doc =
  doc { rootBlkDc = mapBlockTree f (rootBlkDc doc) }

-- | Map over blocks with full traversal context while preserving the original
-- tree shape. Contexts are computed from the original tree.
--
-- If the mapper returns a block with its own children, those children are
-- discarded and replaced by the recursively mapped original children.
mapBlocksWithCtx :: (BlockCtx d b -> Block b) -> HBDoc d b -> HBDoc d b
mapBlocksWithCtx f doc =
  let (_, rootBlk') = mapFromCtx f (mkRootCtx doc)
  in doc { rootBlkDc = rootBlk' }

-- | Find the first block context matching the predicate in canonical pre-order.
findBlockCtx :: (BlockCtx d b -> Bool) -> HBDoc d b -> Maybe (BlockCtx d b)
findBlockCtx predicate doc =
  findFromCtxPreorder predicate (mkRootCtx doc)

-- | Filter block contexts in canonical pre-order.
filterBlockCtxs :: (BlockCtx d b -> Bool) -> HBDoc d b -> [BlockCtx d b]
filterBlockCtxs predicate doc =
  filterFromCtxPreorder predicate (mkRootCtx doc)

-- | Lookup one block context by canonical path.
-- The resulting context preserves document-global path, ancestry, sibling
-- metadata, and pre-order index.
lookupCtxByPath :: PathBlk -> HBDoc d b -> Maybe (BlockCtx d b)
lookupCtxByPath path doc =
  lookupFromCtxPath (pathToList path) (mkRootCtx doc)

-- Internal helpers

collectFromCtxPreorder :: BlockCtx d b -> [BlockCtx d b]
collectFromCtxPreorder ctx =
  let step acc item = acc <> Endo (item :)
  in appEndo (snd (foldFromCtxPreorder step mempty ctx)) []

collectFromCtxPostorder :: BlockCtx d b -> [BlockCtx d b]
collectFromCtxPostorder ctx =
  let step acc item = acc <> Endo (item :)
  in appEndo (snd (foldFromCtxPostorder step mempty ctx)) []

filterFromCtxPreorder :: (BlockCtx d b -> Bool) -> BlockCtx d b -> [BlockCtx d b]
filterFromCtxPreorder predicate ctx =
  let step acc item =
        if predicate item
          then acc <> Endo (item :)
          else acc
  in appEndo (snd (foldFromCtxPreorder step mempty ctx)) []

foldFromCtxPreorder :: (acc -> BlockCtx d b -> acc) -> acc -> BlockCtx d b -> (Int32, acc)
foldFromCtxPreorder step acc0 ctx =
  let acc1 = step acc0 ctx
      blk = ctx.blockBc
      kids = blk.childrenBk
      childCount = fromIntegral (length kids)
      PreorderIx pre0 = ctx.preorderIxBc
      stepChild (nextPre, acc) (ix, childBlk) =
        let childCtx = mkChildCtx ctx (IndexBlk ix) childCount (PreorderIx nextPre) childBlk
        in foldFromCtxPreorder step acc childCtx
  in foldl' stepChild (pre0 + 1, acc1) (zip [0 :: Int32 ..] kids)

foldFromCtxPostorder :: (acc -> BlockCtx d b -> acc) -> acc -> BlockCtx d b -> (Int32, acc)
foldFromCtxPostorder step acc0 ctx =
  let blk = ctx.blockBc
      kids = blk.childrenBk
      childCount = fromIntegral (length kids)
      PreorderIx pre0 = ctx.preorderIxBc
      stepChild (nextPre, acc) (ix, childBlk) =
        let childCtx = mkChildCtx ctx (IndexBlk ix) childCount (PreorderIx nextPre) childBlk
        in foldFromCtxPostorder step acc childCtx
      (nextPre, acc1) = foldl' stepChild (pre0 + 1, acc0) (zip [0 :: Int32 ..] kids)
  in (nextPre, step acc1 ctx)

findFromCtxPreorder :: (BlockCtx d b -> Bool) -> BlockCtx d b -> Maybe (BlockCtx d b)
findFromCtxPreorder predicate ctx =
  case iterContext ctx of
    Right found -> Just found
    Left _ -> Nothing
  where
    iterContext ctx0
      | predicate ctx0 = Right ctx0
      | otherwise =
          let blk0 = ctx0.blockBc
              kids0 = blk0.childrenBk
              childCount0 = fromIntegral (length kids0)
              PreorderIx pre0 = ctx0.preorderIxBc

              goChildren nextPre pairs =
                case pairs of
                  [] -> Left nextPre
                  (ix, childBlk) : rest ->
                    let childCtx = mkChildCtx ctx0 (IndexBlk ix) childCount0 (PreorderIx nextPre) childBlk
                    in case iterContext childCtx of
                         Right found -> Right found
                         Left nextPre' -> goChildren nextPre' rest
          in goChildren (pre0 + 1) (zip [0 :: Int32 ..] kids0)

lookupFromCtxPath :: [IndexBlk] -> BlockCtx d b -> Maybe (BlockCtx d b)
lookupFromCtxPath pathSegs ctx =
  case pathSegs of
    [] -> Just ctx
    ix : rest ->
      case childCtxAt ix ctx of
        Nothing -> Nothing
        Just childCtx -> lookupFromCtxPath rest childCtx


childCtxAt :: IndexBlk -> BlockCtx d b -> Maybe (BlockCtx d b)
childCtxAt wantedIx parentCtx =
  let
    IndexBlk wanted = wantedIx
    parentBlk = parentCtx.blockBc
    kids = parentBlk.childrenBk
    childCount = fromIntegral (length kids)
    PreorderIx parentPre = parentCtx.preorderIxBc

    iterBlock :: BlockCtx d b -> Int32 -> [(Int32, Block b)] -> Maybe (BlockCtx d b)
    iterBlock ctx nextPre pairs =
      case pairs of
        [] -> Nothing
        (ix, childBlk) : rest -> if ix == wanted then
            Just (mkChildCtx ctx (IndexBlk ix) childCount (PreorderIx nextPre) childBlk)
          else
            iterBlock ctx (nextPre + countNodesBlk childBlk) rest
  in
  if wanted < 0 then
    Nothing
  else
    iterBlock parentCtx (parentPre + 1) (zip [0 :: Int32 ..] kids)


mkRootCtx :: HBDoc d b -> BlockCtx d b
mkRootCtx doc =
  let
    rootBlk = doc.rootBlkDc
    rootPth = rootPath
  in 
  BlockCtx {
      docTitleBc = titleDc doc
    , docFormatBc = formatDc doc
    , docMetaBc = metaDefDc doc
    , docExtraBc = extraDc doc
    , blockBc = rootBlk
    , pathBc = rootPth
    , parentPathBc = Nothing
    , depthBc = pathDepth rootPth
    , preorderIxBc = PreorderIx 0
    , siblingIxBc = IndexBlk 0
    , siblingCountBc = 1
    , ancestorsBc = []
    , headingPathBc = Nothing
    , listPathBc = Nothing
    , listItemPathBc = Nothing
    , tablePathBc = Nothing
    , notePathBc = Nothing
    , conversationPathBc = Nothing
    , customPathBc = Nothing
    , customTagBc = Nothing
    }

-- | Ancestors are stored nearest-first:
-- immediate parent at the head, root last.
mkChildCtx :: BlockCtx d b -> IndexBlk -> Int32 -> PreorderIx -> Block b -> BlockCtx d b
mkChildCtx parentCtx childIx childCount childPre childBlk =
  let 
    childPth = appendChildPath (parentCtx.pathBc) childIx
    parentPth = parentCtx.pathBc
    parentBlk = parentCtx.blockBc
  in
  BlockCtx {
      docTitleBc = parentCtx.docTitleBc
    , docFormatBc = parentCtx.docFormatBc
    , docMetaBc = parentCtx.docMetaBc
    , docExtraBc = parentCtx.docExtraBc
    , blockBc = childBlk
    , pathBc = childPth
    , parentPathBc = Just parentPth
    , depthBc = pathDepth childPth
    , preorderIxBc = childPre
    , siblingIxBc = childIx
    , siblingCountBc = childCount
    , ancestorsBc = mkAncestorFrame parentPth parentBlk : parentCtx.ancestorsBc
    , headingPathBc = Nothing
    , listPathBc = Nothing
    , listItemPathBc = Nothing
    , tablePathBc = Nothing
    , notePathBc = Nothing
    , conversationPathBc = Nothing
    , customPathBc = Nothing
    , customTagBc = Nothing
    }


mkAncestorFrame :: PathBlk -> Block b -> AncestorFrame b
mkAncestorFrame path blk = AncestorFrame {
    pathAf = path
  , depthAf = pathDepth path
  , kindAf = kindBk blk
  , contentAf = contentBk blk
  , semAf = semBk blk
  , attribsAf = attribsBk blk
  , provenanceAf = provenanceBk blk
  , childCountAf = fromIntegral (length (childrenBk blk))
  , extraAf = blk.extraBk
  }


mapBlockTree :: (Block b -> Block b) -> Block b -> Block b
mapBlockTree f blk =
  let
    kids' = map (mapBlockTree f) (childrenBk blk)
    blk' = f blk
  in
  blk' { childrenBk = kids' }


mapFromCtx :: (BlockCtx d b -> Block b) -> BlockCtx d b -> (Int32, Block b)
mapFromCtx f ctx =
  let
    blk0 = ctx.blockBc
    kids0 = blk0.childrenBk
    childCount0 = fromIntegral (length kids0)
    PreorderIx pre0 = ctx.preorderIxBc

    stepChild (nextPre, kidsRev) (ix, childBlk) =
      let childCtx = mkChildCtx ctx (IndexBlk ix) childCount0 (PreorderIx nextPre) childBlk
          (nextPre', childBlk') = mapFromCtx f childCtx
      in (nextPre', childBlk' : kidsRev)

    (nextPre, kidsRev) = foldl' stepChild (pre0 + 1, []) (zip [0 :: Int32 ..] kids0)
    blk1 = f ctx
  in
  (nextPre, blk1 { childrenBk = reverse kidsRev })


countNodesBlk :: Block b -> Int32
countNodesBlk blk =
  1 + sum (map countNodesBlk (childrenBk blk))