{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{- HLINT ignore "Use list comprehension" -}

module HBDoc.Analysis.Features where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import HBDoc.Analysis.Context
import HBDoc.Analysis.Flatten
import HBDoc.Analysis.Path
import HBDoc.Analysis.Traverse
import HBDoc.Analysis.Types
import HBDoc.Core.Attributes
import HBDoc.Core.BlockKind
import HBDoc.Core.ListSemantics
import HBDoc.Core.Provenance
import HBDoc.Core.Semantics
import HBDoc.Core.Types
import Data.Maybe (isJust)

data SemProj = SemProj
  { semTagSp :: !(Maybe Text)
  , headingLevelSp :: !(Maybe Int32)
  , headingLabelRawSp :: !(Maybe Text)
  , headingSourceSp :: !(Maybe HeadingSource)
  , listKindSp :: !(Maybe ListKind)
  , listSourceSp :: !(Maybe ListSource)
  , listStartAtSp :: !(Maybe Int32)
  , listIsTightSp :: !(Maybe Bool)
  , listContinuesPrevSp :: !(Maybe Bool)
  , listSeriesIdSp :: !(Maybe Text)
  , listItemMarkerRawSp :: !(Maybe Text)
  , listItemMarkerKindSp :: !(Maybe Text)
  , listItemMarkerSourceSp :: !(Maybe MarkerSource)
  , listItemPresentationLevelSp :: !(Maybe Int32)
  , listItemInferredSp :: !(Maybe Bool)
  , codeLanguageSp :: !(Maybe Text)
  , codeInfoStringSp :: !(Maybe Text)
  , tableHeaderRowsSp :: !(Maybe Int32)
  , tableCaptionSp :: !(Maybe Text)
  , tableCellRowSpanSp :: !(Maybe Int32)
  , tableCellColSpanSp :: !(Maybe Int32)
  , figureCaptionSp :: !(Maybe Text)
  , imageAltTextSp :: !(Maybe Text)
  , noteKindSp :: !(Maybe NoteKind)
  , noteIdSp :: !(Maybe Int32)
  , conversationThreadIdSp :: !(Maybe Text)
  , messageRoleSp :: !(Maybe Text)
  , messageAuthorSp :: !(Maybe Text)
  , messageTsSp :: !(Maybe Text)
  , customTagSp :: !(Maybe Text)
  , customFieldsCountSp :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data AttrProj = AttrProj
  { captionAp :: !(Maybe Text)
  , styleNameAp :: !(Maybe Text)
  , styleIdAp :: !(Maybe Text)
  , numberingNumIdAp :: !(Maybe Int32)
  , numberingLevelAp :: !(Maybe Int32)
  , numberingFormatAp :: !(Maybe Text)
  , numberingTextAp :: !(Maybe Text)
  , numberingStartAtAp :: !(Maybe Int32)
  , mediaRidAp :: !(Maybe Text)
  , mediaNameAp :: !(Maybe Text)
  , mediaKeyAp :: !(Maybe Text)
  , mediaWidthEmuAp :: !(Maybe Int32)
  , mediaHeightEmuAp :: !(Maybe Int32)
  , noteRefIdAp :: !(Maybe Int32)
  , noteRefKindAp :: !(Maybe NoteKind)
  , htmlIdAp :: !(Maybe Text)
  , htmlClassesAp :: ![Text]
  , htmlClassCountAp :: !Int32
  , htmlAttrCountAp :: !Int32
  , customAttrCountAp :: !Int32
  , customAttrKeysAp :: ![Text]
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ProvProj = ProvProj
  { structureSourcePp :: !(Maybe StructureSource)
  , sourceRefPp :: !(Maybe Text)
  , inferenceKindPp :: !(Maybe InferenceKind)
  , confidencePp :: !(Maybe Double)
  , isInferredPp :: !Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data BlockFeatures = BlockFeatures
  { docTitleBf :: !Text
  , docFormatBf :: !(Maybe Text)
  , pathBf :: !PathBlk
  , parentPathBf :: !(Maybe PathBlk)
  , depthBf :: !DepthBlk
  , preorderIxBf :: !PreorderIx
  , siblingIxBf :: !IndexBlk
  , siblingCountBf :: !Int32
  , kindBf :: !KindBlk
  , isLeafBf :: !Bool
  , childCountBf :: !Int32
  , descendantCountBf :: !Int32
  , subtreeSizeBf :: !Int32
  , localTextPresentBf :: !Bool
  , localTextBf :: !(Maybe Text)
  , localTextLenBf :: !Int32
  , subtreeTextLenBf :: !Int32
  , headingPathTextBf :: !Text
  , sectionTextLenBf :: !Int32
  , semProjBf :: !SemProj
  , attribProjBf :: !AttrProj
  , provenanceProjBf :: !ProvProj
  , nearestHeadingPathBf :: !(Maybe PathBlk)
  , nearestHeadingLevelBf :: !(Maybe Int32)
  , nearestListPathBf :: !(Maybe PathBlk)
  , nearestListItemPathBf :: !(Maybe PathBlk)
  , nearestTablePathBf :: !(Maybe PathBlk)
  , nearestConversationPathBf :: !(Maybe PathBlk)
  , nearestMessagePathBf :: !(Maybe PathBlk)
  , nearestNotePathBf :: !(Maybe PathBlk)
  , nearestCustomPathBf :: !(Maybe PathBlk)
  , headingChainPathsBf :: ![PathBlk]
  , headingChainTextsBf :: ![Maybe Text]
  , listDepthBf :: !Int32
  , insideDefinitionListBf :: !Bool
  , insideNoteBf :: !Bool
  , insideConversationBf :: !Bool
  , insideMessageBf :: !Bool
  , insideCustomBf :: !Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data DocFeatures = DocFeatures
  { titleDf :: !Text
  , formatDf :: !(Maybe Text)
  , rootChildCountDf :: !Int32
  , totalBlockCountDf :: !Int32
  , countByKindDf :: ![(KindBlk, Int32)]
  , headingCountByLevelDf :: ![(Int32, Int32)]
  , listCountByKindDf :: ![(ListKind, Int32)]
  , noteCountDf :: !Int32
  , messageCountDf :: !Int32
  , conversationCountDf :: !Int32
  , tableCountDf :: !Int32
  , figureCountDf :: !Int32
  , imageCountDf :: !Int32
  , customCountDf :: !Int32
  , inferredBlockCountDf :: !Int32
  , provenanceCountBySourceDf :: ![(StructureSource, Int32)]
  , totalVisibleTextLenDf :: !Int32
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

emptySemProj :: SemProj
emptySemProj =
  SemProj
    { semTagSp = Nothing
    , headingLevelSp = Nothing
    , headingLabelRawSp = Nothing
    , headingSourceSp = Nothing
    , listKindSp = Nothing
    , listSourceSp = Nothing
    , listStartAtSp = Nothing
    , listIsTightSp = Nothing
    , listContinuesPrevSp = Nothing
    , listSeriesIdSp = Nothing
    , listItemMarkerRawSp = Nothing
    , listItemMarkerKindSp = Nothing
    , listItemMarkerSourceSp = Nothing
    , listItemPresentationLevelSp = Nothing
    , listItemInferredSp = Nothing
    , codeLanguageSp = Nothing
    , codeInfoStringSp = Nothing
    , tableHeaderRowsSp = Nothing
    , tableCaptionSp = Nothing
    , tableCellRowSpanSp = Nothing
    , tableCellColSpanSp = Nothing
    , figureCaptionSp = Nothing
    , imageAltTextSp = Nothing
    , noteKindSp = Nothing
    , noteIdSp = Nothing
    , conversationThreadIdSp = Nothing
    , messageRoleSp = Nothing
    , messageAuthorSp = Nothing
    , messageTsSp = Nothing
    , customTagSp = Nothing
    , customFieldsCountSp = 0
    }

emptyAttrProj :: AttrProj
emptyAttrProj =
  AttrProj
    { captionAp = Nothing
    , styleNameAp = Nothing
    , styleIdAp = Nothing
    , numberingNumIdAp = Nothing
    , numberingLevelAp = Nothing
    , numberingFormatAp = Nothing
    , numberingTextAp = Nothing
    , numberingStartAtAp = Nothing
    , mediaRidAp = Nothing
    , mediaNameAp = Nothing
    , mediaKeyAp = Nothing
    , mediaWidthEmuAp = Nothing
    , mediaHeightEmuAp = Nothing
    , noteRefIdAp = Nothing
    , noteRefKindAp = Nothing
    , htmlIdAp = Nothing
    , htmlClassesAp = []
    , htmlClassCountAp = 0
    , htmlAttrCountAp = 0
    , customAttrCountAp = 0
    , customAttrKeysAp = []
    }

emptyProvProj :: ProvProj
emptyProvProj =
  ProvProj
    { structureSourcePp = Nothing
    , sourceRefPp = Nothing
    , inferenceKindPp = Nothing
    , confidencePp = Nothing
    , isInferredPp = False
    }

extractBlockFeatures :: FlattenCfg -> HBDoc d b -> BlockCtx d b -> BlockFeatures
extractBlockFeatures cfg doc ctx =
  extractBlockFeaturesWithDescCounts cfg doc M.empty ctx

extractAllBlockFeatures :: FlattenCfg -> HBDoc d b -> [BlockFeatures]
extractAllBlockFeatures cfg doc =
  let preorderCtxs = walkBlocksPreorder doc
      descendantCounts = descendantCountMapDoc doc
  in map (extractBlockFeaturesWithDescCounts cfg doc descendantCounts) preorderCtxs

extractDocFeatures :: FlattenCfg -> HBDoc d b -> DocFeatures
extractDocFeatures cfg doc =
  let ctxs = walkBlocksPreorder doc
      kindCounts = kindCountsCtxs ctxs
      headingCounts = headingLevelCountsCtxs ctxs
      listCounts = listKindCountsCtxs ctxs
      provenanceCounts = provenanceSourceCountsCtxs ctxs
  in DocFeatures
      { titleDf = titleDc doc
      , formatDf = formatDc doc
      , rootChildCountDf = int32Length (childrenBk (rootBlkDc doc))
      , totalBlockCountDf = int32Length ctxs
      , countByKindDf = M.toAscList kindCounts
      , headingCountByLevelDf = M.toAscList headingCounts
      , listCountByKindDf = M.toAscList listCounts
      , noteCountDf = countCtxsWhere (\ctx -> isNoteKindKb ctx.blockBc.kindBk) ctxs
      , messageCountDf = countCtxsWhere (\ctx -> ctx.blockBc.kindBk == MessageKB) ctxs
      , conversationCountDf = countCtxsWhere (\ctx -> ctx.blockBc.kindBk == ConversationKB) ctxs
      , tableCountDf = countCtxsWhere (\ctx -> ctx.blockBc.kindBk == TableKB) ctxs
      , figureCountDf = countCtxsWhere (\ctx -> ctx.blockBc.kindBk == FigureKB) ctxs
      , imageCountDf = countCtxsWhere (\ctx -> ctx.blockBc.kindBk == ImageKB) ctxs
      , customCountDf = countCtxsWhere (\ctx -> isCustomKindKb ctx.blockBc.kindBk) ctxs
      , inferredBlockCountDf =
          countCtxsWhere
            (\ctx ->
              case ctx.blockBc.provenanceBk of
                Nothing -> False
                Just prov -> case prov.inferencePB of
                  Just _ -> True
                  Nothing -> False
            )
            ctxs
      , provenanceCountBySourceDf = M.toAscList provenanceCounts
      , totalVisibleTextLenDf = textLen (flattenDocText cfg doc)
      }

projectSemanticsBk :: Block b -> SemProj
projectSemanticsBk blk =
  case semBk blk of
    Nothing ->
      semProjFromKindFallback (kindBk blk)

    Just sem ->
      semProjFromSem sem

projectAttributesBk :: Block b -> AttrProj
projectAttributesBk blk =
  let attribs = attribsBk blk
      styleAttrs = styleAb attribs
      numberingAttrs = numberingAb attribs
      mediaAttrs = mediaAb attribs
      noteRefAttrs = noteRefAb attribs
      htmlAttrs = htmlAb attribs
      htmlClasses =
        case htmlAttrs of
          Just attrs -> htmlClassesHa attrs
          Nothing -> []
      htmlAttrCount =
        case htmlAttrs of
          Just attrs -> mapSizeInt32 (htmlAttrsHa attrs)
          Nothing -> 0
      customAttrs = customAb attribs
  in AttrProj
      { captionAp = captionAb attribs
      , styleNameAp = styleAttrs >>= nameSA
      , styleIdAp = styleAttrs >>= idSA
      , numberingNumIdAp = numberingAttrs >>= numIdNA
      , numberingLevelAp = numberingAttrs >>= ilvlNA
      , numberingFormatAp = numberingAttrs >>= numFmtNA
      , numberingTextAp = numberingAttrs >>= lvlTextNA
      , numberingStartAtAp = numberingAttrs >>= startAtNA
      , mediaRidAp = mediaAttrs >>= imageRidMa
      , mediaNameAp = mediaAttrs >>= imageNameMa
      , mediaKeyAp = mediaAttrs >>= imageKeyMa
      , mediaWidthEmuAp = mediaAttrs >>= widthEmuMa
      , mediaHeightEmuAp = mediaAttrs >>= heightEmuMa
      , noteRefIdAp = noteRefAttrs >>= noteIdNra
      , noteRefKindAp = noteRefAttrs >>= noteKindNra
      , htmlIdAp = htmlAttrs >>= htmlIdHa
      , htmlClassesAp = htmlClasses
      , htmlClassCountAp = int32Length htmlClasses
      , htmlAttrCountAp = htmlAttrCount
      , customAttrCountAp = mapSizeInt32 customAttrs
      , customAttrKeysAp = M.keys customAttrs
      }

projectProvenanceBk :: Block b -> ProvProj
projectProvenanceBk blk =
  case provenanceBk blk of
    Nothing ->
      emptyProvProj

    Just prov ->
      ProvProj
        { structureSourcePp = Just (sourcePB prov)
        , sourceRefPp = sourceRefPB prov
        , inferenceKindPp = inferencePB prov
        , confidencePp = confidencePB prov
        , isInferredPp =
            case inferencePB prov of
              Just _ -> True
              Nothing -> False
        }

countDescendants :: BlockCtx d b -> Int32
countDescendants ctx = subtreeSizeBlk ctx.blockBc - 1

countKindsDoc :: HBDoc d b -> Map KindBlk Int32
countKindsDoc doc =
  kindCountsCtxs (walkBlocksPreorder doc)

extractBlockFeaturesWithDescCounts ::
  FlattenCfg ->
  HBDoc d b ->
  Map PathBlk Int32 ->
  BlockCtx d b ->
  BlockFeatures
extractBlockFeaturesWithDescCounts cfg doc descendantCounts ctx =
  let
    blk = ctx.blockBc
    localTxt = contentBk blk
    childCount = int32Length (childrenBk blk)
    descendantCount = M.findWithDefault (countDescendants ctx) ctx.pathBc descendantCounts
    headingChain = ensureCurrentHeadingCtx ctx (headingChainCtx ctx)
    nearestHeading = deepestCtxByPath headingChain
    nearestListPath =
      case kindBk blk of
        ListKB -> Just ctx.pathBc
        _ -> nearestListCtx ctx >>= Just . pathBc
    nearestListItemPath =
      case kindBk blk of
        ListItemKB -> Just ctx.pathBc
        _ -> nearestListItemCtx ctx >>= Just . pathBc
    nearestTablePath = nearestSelfOrAncestorPath (== TableKB) ctx
    nearestConversationPath = nearestSelfOrAncestorPath (== ConversationKB) ctx
    nearestMessagePath = nearestSelfOrAncestorPath (== MessageKB) ctx
    nearestNotePath = nearestSelfOrAncestorPath isNoteKindKb ctx
    nearestCustomPath = nearestSelfOrAncestorPath isCustomKindKb ctx
    nearestListKind =
      case kindBk blk of
        ListKB -> listKindBlock blk
        _ -> nearestListCtx ctx >>= listKindCtx
    insideDefinitionList =
      case nearestListKind of
        Just DefinitionLK -> True
        _ -> False
    -- TODO: verify if this is correct:
    listDepth = maybe 0 (fromIntegral . calcPathDepth) ctx.listPathBc
  in
  BlockFeatures {
    docTitleBf = doc.titleDc
    , docFormatBf = doc.formatDc
    , pathBf = ctx.pathBc
    , parentPathBf = ctx.parentPathBc
    , depthBf = ctx.depthBc
    , preorderIxBf = ctx.preorderIxBc
    , siblingIxBf = ctx.siblingIxBc
    , siblingCountBf = ctx.siblingCountBc
    , kindBf = kindBk blk
    , isLeafBf = null (childrenBk blk)
    , childCountBf = childCount
    , descendantCountBf = descendantCount
    , subtreeSizeBf = descendantCount + 1
    , localTextPresentBf =
        case localTxt of
          Just _ -> True
          Nothing -> False
    , localTextBf = localTxt
    , localTextLenBf = textLenMaybe localTxt
    , subtreeTextLenBf = textLen (flattenSubtreeText cfg ctx)
    , headingPathTextBf = flattenHeadingPathText cfg ctx
    , sectionTextLenBf = textLen (flattenSectionText cfg ctx)
    , semProjBf = projectSemanticsBk blk
    , attribProjBf = projectAttributesBk blk
    , provenanceProjBf = projectProvenanceBk blk
    , nearestHeadingPathBf = nearestHeading >>= Just . pathBc
    , nearestHeadingLevelBf = nearestHeading >>= headingLevelCtx
    , nearestListPathBf = nearestListPath
    , nearestListItemPathBf = nearestListItemPath
    , nearestTablePathBf = nearestTablePath
    , nearestConversationPathBf = nearestConversationPath
    , nearestMessagePathBf = nearestMessagePath
    , nearestNotePathBf = nearestNotePath
    , nearestCustomPathBf = nearestCustomPath
    , headingChainPathsBf = map pathBc headingChain
    , headingChainTextsBf = map (contentBk . blockBc) headingChain
    , listDepthBf = listDepth
    , insideDefinitionListBf = insideDefinitionList
    , insideNoteBf = isJust nearestNotePath
    , insideConversationBf = isJust nearestConversationPath
    , insideMessageBf = isJust nearestMessagePath
    , insideCustomBf = isJust nearestCustomPath
      }


calcPathDepth :: PathBlk -> Int
calcPathDepth (PathBlk paths) =
  length paths

semProjFromKindFallback :: KindBlk -> SemProj
semProjFromKindFallback kind =
  case kind of
    HeadingKB ->
      emptySemProj { semTagSp = Just "heading" }

    ListKB ->
      emptySemProj { semTagSp = Just "list" }

    ListItemKB ->
      emptySemProj { semTagSp = Just "list-item" }

    CodeKB ->
      emptySemProj { semTagSp = Just "code" }

    TableKB ->
      emptySemProj { semTagSp = Just "table" }

    TableCellKB ->
      emptySemProj { semTagSp = Just "table-cell" }

    FigureKB ->
      emptySemProj { semTagSp = Just "figure" }

    ImageKB ->
      emptySemProj { semTagSp = Just "image" }

    NoteKB noteKind ->
      emptySemProj
        { semTagSp = Just "note"
        , noteKindSp = Just noteKind
        }

    ConversationKB ->
      emptySemProj { semTagSp = Just "conversation" }

    MessageKB ->
      emptySemProj { semTagSp = Just "message" }

    CustomKB tag ->
      emptySemProj
        { semTagSp = Just "custom"
        , customTagSp = Just tag
        }

    _ ->
      emptySemProj

semProjFromSem :: SemanticsBlk -> SemProj
semProjFromSem sem =
  case sem of
    HeadingSB headingSem ->
      emptySemProj
        { semTagSp = Just "heading"
        , headingLevelSp = Just (levelHdg headingSem)
        , headingLabelRawSp = rawLbl <$> labelHdg headingSem
        , headingSourceSp = Just (sourceHdg headingSem)
        }

    ListSB listSem ->
      emptySemProj
        { semTagSp = Just "list"
        , listKindSp = Just (kindLs listSem)
        , listSourceSp = Just (sourceLs listSem)
        , listStartAtSp = startAtLs listSem
        , listIsTightSp = isTightLs listSem
        , listContinuesPrevSp = continuesPrevLs listSem
        , listSeriesIdSp = seriesIdLs listSem
        }

    ListItemSB listItemSem ->
      emptySemProj
        { semTagSp = Just "list-item"
        , listItemMarkerRawSp =
            rawMarkerLsi listItemSem <|> (markerLsi listItemSem >>= markerRawText)
        , listItemMarkerKindSp = markerKindText <$> markerLsi listItemSem
        , listItemMarkerSourceSp = Just (markerSourceLsi listItemSem)
        , listItemPresentationLevelSp = presentationLsi listItemSem >>= presentationLevel
        , listItemInferredSp = Just (isInferredLsi listItemSem)
        }

    CodeSB codeSem ->
      emptySemProj
        { semTagSp = Just "code"
        , codeLanguageSp = languageCds codeSem
        , codeInfoStringSp = infoStringCds codeSem
        }

    TableSB tableSem ->
      emptySemProj
        { semTagSp = Just "table"
        , tableHeaderRowsSp = headerRowsTbs tableSem
        , tableCaptionSp = captionTbs tableSem
        }

    TableCellSB tableCellSem ->
      emptySemProj
        { semTagSp = Just "table-cell"
        , tableCellRowSpanSp = rowSpanTcs tableCellSem
        , tableCellColSpanSp = colSpanTcs tableCellSem
        }

    FigureSB figureSem ->
      emptySemProj
        { semTagSp = Just "figure"
        , figureCaptionSp = captionFgs figureSem
        }

    ImageSB imageSem ->
      emptySemProj
        { semTagSp = Just "image"
        , imageAltTextSp = altTextIms imageSem
        }

    NoteSB noteSem ->
      emptySemProj
        { semTagSp = Just "note"
        , noteKindSp = Just (kindNts noteSem)
        , noteIdSp = noteIdNts noteSem
        }

    ConversationSB conversationSem ->
      emptySemProj
        { semTagSp = Just "conversation"
        , conversationThreadIdSp = threadIdCvs conversationSem
        }

    MessageSB messageSem ->
      emptySemProj
        { semTagSp = Just "message"
        , messageRoleSp = roleMsg messageSem
        , messageAuthorSp = authorMsg messageSem
        , messageTsSp = tsMsg messageSem
        }

    CustomSB customSem ->
      emptySemProj
        { semTagSp = Just "custom"
        , customTagSp = Just (tagCst customSem)
        , customFieldsCountSp = mapSizeInt32 (fieldsCst customSem)
        }

headingLevelCtx :: BlockCtx d b -> Maybe Int32
headingLevelCtx ctx =
  headingLevelBlock ctx.blockBc

listKindCtx :: BlockCtx d b -> Maybe ListKind
listKindCtx ctx =
  listKindBlock ctx.blockBc

headingLevelBlock :: Block b -> Maybe Int32
headingLevelBlock blk =
  case semBk blk of
    Just (HeadingSB headingSem) -> Just (levelHdg headingSem)
    _ -> Nothing

listKindBlock :: Block b -> Maybe ListKind
listKindBlock blk =
  case semBk blk of
    Just (ListSB listSem) -> Just (kindLs listSem)
    _ -> Nothing

markerKindText :: ListMarker -> Text
markerKindText marker =
  case marker of
    MarkerLabelled _ -> "labelled"
    MarkerGraphic _ -> "graphic"
    MarkerNativeOrdered _ -> "native-ordered"
    MarkerNativeBullet -> "native-bullet"
    MarkerNativeDefinition -> "native-definition"

markerRawText :: ListMarker -> Maybe Text
markerRawText marker =
  case marker of
    MarkerLabelled label -> Just (rawLbl label)
    _ -> Nothing

presentationLevel :: ItemPresentation -> Maybe Int32
presentationLevel presentation =
  case presentation of
    HeadingPresentationIP lvl -> Just lvl

kindCountsCtxs :: [BlockCtx d b] -> Map KindBlk Int32
kindCountsCtxs ctxs =
  foldl'
    (\acc ctx -> bumpCount (ctx.blockBc.kindBk) acc)
    M.empty
    ctxs

headingLevelCountsCtxs :: [BlockCtx d b] -> Map Int32 Int32
headingLevelCountsCtxs ctxs =
  foldl'
    (\acc ctx ->
      case headingLevelBlock ctx.blockBc of
        Just lvl -> bumpCount lvl acc
        Nothing -> acc
    )
    M.empty
    ctxs

listKindCountsCtxs :: [BlockCtx d b] -> Map ListKind Int32
listKindCountsCtxs ctxs =
  foldl'
    (\acc ctx ->
      case listKindBlock ctx.blockBc of
        Just listKind -> bumpCount listKind acc
        Nothing -> acc
    )
    M.empty
    ctxs

provenanceSourceCountsCtxs :: [BlockCtx d b] -> Map StructureSource Int32
provenanceSourceCountsCtxs ctxs =
  foldl'
    (\acc ctx ->
      case provenanceBk ctx.blockBc of
        Just prov -> bumpCount (sourcePB prov) acc
        Nothing -> acc
    )
    M.empty
    ctxs

descendantCountMapDoc :: HBDoc d b -> Map PathBlk Int32
descendantCountMapDoc doc =
  foldl' step M.empty (walkBlocksPostorder doc)
  where
  step :: Map PathBlk Int32 -> BlockCtx d b -> Map PathBlk Int32
  step acc ctx =
    let
      childPaths =
          [ appendChildPath ctx.pathBc (IndexBlk childIx)
          | (childIx, _) <- zip [0 :: Int32 ..] ctx.blockBc.childrenBk
          ]
      descendantCount =
          foldl'
            (\n childPath -> n + 1 + M.findWithDefault 0 childPath acc)
            0
            childPaths
    in
    M.insert ctx.pathBc descendantCount acc

subtreeSizeBlk :: Block b -> Int32
subtreeSizeBlk blk =
  1 + foldl' (\n child -> n + subtreeSizeBlk child) 0 (childrenBk blk)

nearestSelfOrAncestorPath :: (KindBlk -> Bool) -> BlockCtx d b -> Maybe PathBlk
nearestSelfOrAncestorPath predKind ctx =
  let
    selfPaths = if predKind ctx.blockBc.kindBk then [ctx.pathBc] else []
    ancestorPaths = [ frame.pathAf | frame <- ctx.ancestorsBc, predKind frame.kindAf ]
  in deepestPath (selfPaths <> ancestorPaths)

ensureCurrentHeadingCtx :: BlockCtx d b -> [BlockCtx d b] -> [BlockCtx d b]
ensureCurrentHeadingCtx ctx chain =
  case headingLevelBlock ctx.blockBc of
    Nothing ->
      chain

    Just _ ->
      case deepestCtxByPath chain of
        Just deepestHeading
          | deepestHeading.pathBc == ctx.pathBc -> chain
        _ -> chain <> [ctx]

deepestPath :: [PathBlk] -> Maybe PathBlk
deepestPath paths =
  foldl' step Nothing paths
  where
    step best path =
      case best of
        Nothing -> Just path
        Just bestPath ->
          if pathDepth path >= pathDepth bestPath
            then Just path
            else Just bestPath

deepestCtxByPath :: [BlockCtx d b] -> Maybe (BlockCtx d b)
deepestCtxByPath ctxs =
  foldl' step Nothing ctxs
  where
  step :: Maybe (BlockCtx d b) -> BlockCtx d b -> Maybe (BlockCtx d b)
  step best ctx =
    case best of
      Nothing -> Just ctx
      Just bestCtx ->
        if pathDepth ctx.pathBc >= pathDepth bestCtx.pathBc
          then Just ctx
          else Just bestCtx


isNoteKindKb :: KindBlk -> Bool
isNoteKindKb kind =
  case kind of
    NoteKB _ -> True
    _ -> False

isCustomKindKb :: KindBlk -> Bool
isCustomKindKb kind =
  case kind of
    CustomKB _ -> True
    _ -> False

bumpCount :: Ord k => k -> Map k Int32 -> Map k Int32
bumpCount key counts =
  M.insertWith (+) key 1 counts

countCtxsWhere :: (BlockCtx d b -> Bool) -> [BlockCtx d b] -> Int32
countCtxsWhere predCtx ctxs =
  foldl'
    (\n ctx -> if predCtx ctx then n + 1 else n)
    0
    ctxs

int32Length :: [a] -> Int32
int32Length xs =
  fromIntegral (length xs)

mapSizeInt32 :: Map k v -> Int32
mapSizeInt32 mp =
  fromIntegral (M.size mp)

textLen :: Text -> Int32
textLen txt =
  fromIntegral (T.length txt)

textLenMaybe :: Maybe Text -> Int32
textLenMaybe txt =
  case txt of
    Just t -> textLen t
    Nothing -> 0