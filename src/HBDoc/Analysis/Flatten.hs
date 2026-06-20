{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{- HLINT ignore "Use list comprehension" -}

module HBDoc.Analysis.Flatten (
    FlattenMode(..),
    ChunkKind(..),
    FlattenChunk(..),
    FlattenCfg(..),
    defaultFlattenCfg,
    normalizedFlattenCfg,
    indexingFlattenCfg,
    flattenBlockText,
    flattenSubtreeText,
    flattenDocText,
    flattenBlockChunks,
    flattenSubtreeChunks,
    flattenDocChunks,
    flattenHeadingPathText,
    flattenSectionText,
    flattenListItemText,
    flattenMessageText,
    flattenTableText,
    renderChunksText,
    chunkPaths
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import HBDoc.Analysis.Path (PathBlk, appendChildPath, pathDepth, rootPath)
import HBDoc.Analysis.Types
import HBDoc.Core.Attributes (captionAb)
import HBDoc.Core.BlockKind (KindBlk(..), NoteKind(..))
import HBDoc.Core.ListSemantics
import HBDoc.Core.Semantics
import HBDoc.Core.Types (Block(..), HBDoc(..))
import Data.Ord (Down(..))

-- | Canonical flattening configuration.
--
-- Rules encoded by default:
--
-- * generic block siblings are separated by two newlines
-- * list items are separated by one newline
-- * table cells are separated by " | "
-- * table rows are separated by one newline
-- * heading-path components are separated by " > "
-- * list markers are included
-- * synthetic metadata/captions are excluded by default
-- * source text is preserved exactly by default
data FlattenCfg = FlattenCfg {
    blockSepFc :: !Text
  , childBlockSepFc :: !Text
  , headingPathSepFc :: !Text
  , listItemSepFc :: !Text
  , listMarkerSuffixFc :: !Text
  , tableCellSepFc :: !Text
  , tableRowSepFc :: !Text
  , metaSepFc :: !Text
  , normalizeWhitespaceFc :: !Bool
  , skipEmptyTextFc :: !Bool
  , includeListMarkersFc :: !Bool
  , includeMessageMetaFc :: !Bool
  , includeNoteMetaFc :: !Bool
  , includeImageAltFc :: !Bool
  , includeFigureCaptionFc :: !Bool
  , includeTableCaptionFc :: !Bool
  , includeCustomSemFc :: !Bool
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

defaultFlattenCfg :: FlattenCfg
defaultFlattenCfg =
  FlattenCfg
    { blockSepFc = "\n\n"
    , childBlockSepFc = "\n\n"
    , headingPathSepFc = " > "
    , listItemSepFc = "\n"
    , listMarkerSuffixFc = " "
    , tableCellSepFc = " | "
    , tableRowSepFc = "\n"
    , metaSepFc = "\n"
    , normalizeWhitespaceFc = False
    , skipEmptyTextFc = True
    , includeListMarkersFc = True
    , includeMessageMetaFc = False
    , includeNoteMetaFc = False
    , includeImageAltFc = False
    , includeFigureCaptionFc = False
    , includeTableCaptionFc = False
    , includeCustomSemFc = False
    }

normalizedFlattenCfg :: FlattenCfg
normalizedFlattenCfg =
  defaultFlattenCfg { normalizeWhitespaceFc = True }

indexingFlattenCfg :: FlattenCfg
indexingFlattenCfg =
  defaultFlattenCfg
    { normalizeWhitespaceFc = True
    , includeMessageMetaFc = True
    , includeNoteMetaFc = True
    , includeImageAltFc = True
    , includeFigureCaptionFc = True
    , includeTableCaptionFc = True
    , includeCustomSemFc = True
    }

flattenBlockText :: FlattenCfg -> BlockCtx d b -> Text
flattenBlockText cfg ctx =
  renderChunksText (flattenBlockChunks cfg ctx)

flattenSubtreeText :: FlattenCfg -> BlockCtx d b -> Text
flattenSubtreeText cfg ctx =
  renderChunksText (flattenSubtreeChunks cfg ctx)

flattenDocText :: FlattenCfg -> HBDoc d b -> Text
flattenDocText cfg doc =
  renderChunksText (flattenDocChunks cfg doc)

flattenBlockChunks :: FlattenCfg -> BlockCtx d b -> [FlattenChunk]
flattenBlockChunks cfg ctx =
  flattenBlockSelfChunks cfg (pathBc ctx) (blockBc ctx)

flattenSubtreeChunks :: FlattenCfg -> BlockCtx d b -> [FlattenChunk]
flattenSubtreeChunks cfg ctx =
  flattenSubtreeChunksAt cfg (pathBc ctx) (blockBc ctx)

flattenDocChunks :: FlattenCfg -> HBDoc d b -> [FlattenChunk]
flattenDocChunks cfg doc =
  flattenSubtreeChunksAt cfg rootPath (rootBlkDc doc)

flattenHeadingPathText :: FlattenCfg -> BlockCtx d b -> Text
flattenHeadingPathText cfg ctx =
  let headingGroups =
        map
          (\(path, blk) -> headingOwnChunks cfg path blk)
          (headingPairsFromCtx ctx)
  in renderChunksText (joinChunkGroups (pathBc ctx) (headingPathSepFc cfg) headingGroups)

flattenSectionText :: FlattenCfg -> BlockCtx d b -> Text
flattenSectionText cfg ctx =
  flattenFromNearestOrCurrent cfg isHeadingBlock ctx

flattenListItemText :: FlattenCfg -> BlockCtx d b -> Text
flattenListItemText cfg ctx =
  flattenFromNearestOrCurrent cfg isListItemBlock ctx

flattenMessageText :: FlattenCfg -> BlockCtx d b -> Text
flattenMessageText cfg ctx =
  flattenFromNearestOrCurrent cfg isMessageBlock ctx

flattenTableText :: FlattenCfg -> BlockCtx d b -> Text
flattenTableText cfg ctx =
  flattenFromNearestOrCurrent cfg isTableBlock ctx

renderChunksText :: [FlattenChunk] -> Text
renderChunksText =
  T.concat . map textFch

chunkPaths :: [FlattenChunk] -> [PathBlk]
chunkPaths = map pathFch

flattenSubtreeChunksAt :: FlattenCfg -> PathBlk -> Block b -> [FlattenChunk]
flattenSubtreeChunksAt cfg path blk =
  let selfChunks = flattenBlockSelfChunks cfg path blk
      childGroups =
        [ flattenSubtreeChunksAt cfg (appendChildPath path (IndexBlk idx)) child
        | (idx, child) <- zip [0 :: Int32 ..] (childrenBk blk)
        ]
      childChunks = joinChunkGroups path (siblingSepForParent cfg blk) childGroups
  in joinChunkGroups path (localChildSepForParent cfg blk) [selfChunks, childChunks]

flattenBlockSelfChunks :: FlattenCfg -> PathBlk -> Block b -> [FlattenChunk]
flattenBlockSelfChunks cfg path blk =
  case kindBk blk of
    HeadingKB ->
      contentChunks cfg path HeadingTextCK blk.contentBk

    ParagraphKB ->
      contentChunks cfg path ParagraphTextCK blk.contentBk

    ListKB ->
      -- TODO: find out what is the correct chunk kind for a list:
      contentChunks cfg path UnknownCK blk.contentBk

    ListItemKB ->
      let markerChunks =
            if includeListMarkersFc cfg
              then syntheticChunks cfg path ListMarkerCK (listItemMarkerText blk)
              else []
          bodyChunks =
            contentChunks cfg path ListItemBodyCK blk.contentBk
      in joinChunkGroups path (listMarkerSuffixFc cfg) [markerChunks, bodyChunks]

    QuoteKB ->
      contentChunks cfg path QuoteTextCK blk.contentBk

    CodeKB ->
      contentChunks cfg path CodeTextCK blk.contentBk

    TableKB ->
      let captionChunks =
            if includeTableCaptionFc cfg
              then syntheticChunks cfg path TableCaptionCK (tableCaptionText blk)
              else []
          bodyChunks =
            contentChunks cfg path UnknownCK blk.contentBk
      in joinChunkGroups path (childBlockSepFc cfg) [captionChunks, bodyChunks]

    TableRowKB ->
      contentChunks cfg path UnknownCK blk.contentBk

    TableCellKB ->
      contentChunks cfg path TableCellCK blk.contentBk

    FigureKB ->
      let captionChunks =
            if includeFigureCaptionFc cfg
              then syntheticChunks cfg path FigureCaptionCK (figureCaptionText blk)
              else []
          bodyChunks =
            contentChunks cfg path UnknownCK blk.contentBk
      in joinChunkGroups path (childBlockSepFc cfg) [captionChunks, bodyChunks]

    ImageKB ->
      let altChunks =
            if includeImageAltFc cfg
              then syntheticChunks cfg path ImageAltCK (imageAltText blk)
              else []
          bodyChunks =
            contentChunks cfg path UnknownCK blk.contentBk
      in joinChunkGroups path (childBlockSepFc cfg) [altChunks, bodyChunks]

    RuleKB ->
      []

    NoteKB _ ->
      let metaChunks =
            if includeNoteMetaFc cfg
              then syntheticChunks cfg path NoteTextCK (noteMetaText blk)
              else []
          bodyChunks =
            contentChunks cfg path UnknownCK blk.contentBk
      in joinChunkGroups path (metaSepFc cfg) [metaChunks, bodyChunks]

    ConversationKB ->
      contentChunks cfg path UnknownCK blk.contentBk

    MessageKB ->
      let metaChunks =
            if includeMessageMetaFc cfg then
              syntheticChunks cfg path MessageRoleCK (messageMetaText blk)
            else
              []
          bodyChunks =
            contentChunks cfg path MessageBodyCK blk.contentBk
      in joinChunkGroups path (metaSepFc cfg) [metaChunks, bodyChunks]

    CustomKB _ ->
      let bodyChunks =
            contentChunks cfg path UnknownCK blk.contentBk
          semChunks =
            if includeCustomSemFc cfg then
              syntheticChunks cfg path (CustomTextCK "custom") (customMetaText blk)
            else
              []
      in joinChunkGroups path (childBlockSepFc cfg) [bodyChunks, semChunks]

contentChunks :: FlattenCfg -> PathBlk -> ChunkKind -> Maybe Text -> [FlattenChunk]
contentChunks cfg path chunkKind maybeTxt =
  case maybeTxt of
    Nothing -> []
    Just txt ->
      case mkSourceChunk cfg path chunkKind txt of
        Nothing -> []
        Just chunk -> [chunk]

syntheticChunks :: FlattenCfg -> PathBlk -> ChunkKind -> Maybe Text -> [FlattenChunk]
syntheticChunks cfg path chunkKind maybeTxt =
  case maybeTxt of
    Nothing -> []
    Just txt ->
      case mkSyntheticChunk cfg path chunkKind txt of
        Nothing -> []
        Just chunk -> [chunk]

headingOwnChunks :: FlattenCfg -> PathBlk -> Block b -> [FlattenChunk]
headingOwnChunks cfg path blk =
  contentChunks cfg path HeadingTextCK blk.contentBk

joinChunkGroups :: PathBlk -> Text -> [[FlattenChunk]] -> [FlattenChunk]
joinChunkGroups path sepTxt groups =
  let nonEmptyGroups = filter (not . null) groups
  in case nonEmptyGroups of
       [] -> []
       firstGroup : restGroups -> foldl (\acc group ->
            if T.null sepTxt then
              acc <> group
            else
              acc <> maybeToList (mkSyntheticChunk defaultFlattenCfg path SyntheticSeparatorCK sepTxt) <> group
          ) firstGroup restGroups


mkSourceChunk :: FlattenCfg -> PathBlk -> ChunkKind -> Text -> Maybe FlattenChunk
mkSourceChunk cfg path chunkKind rawTxt =
  let outTxt = applyTextPolicy cfg rawTxt
      offEnd = fromIntegral (T.length rawTxt)
  in if shouldDropText cfg outTxt
       then Nothing
       else Just
         FlattenChunk
           { pathFch = path
           , kindFch = chunkKind
           , textFch = outTxt
           , spanFch = Nothing
           , syntheticFch = False
           }

mkSyntheticChunk :: FlattenCfg -> PathBlk -> ChunkKind -> Text -> Maybe FlattenChunk
mkSyntheticChunk cfg path chunkKind rawTxt =
  let outTxt = applyTextPolicy cfg rawTxt
  in if shouldDropText cfg outTxt
       then Nothing
       else Just
         FlattenChunk
           { pathFch = path
           , kindFch = chunkKind
           , textFch = outTxt
           , spanFch = Nothing
           , syntheticFch = True
           }

applyTextPolicy :: FlattenCfg -> Text -> Text
applyTextPolicy cfg txt =
  if normalizeWhitespaceFc cfg
    then normalizeWhitespace txt
    else txt

shouldDropText :: FlattenCfg -> Text -> Bool
shouldDropText cfg txt =
  skipEmptyTextFc cfg && T.null txt

normalizeWhitespace :: Text -> Text
normalizeWhitespace =
  T.unwords . T.words

siblingSepForParent :: FlattenCfg -> Block b -> Text
siblingSepForParent cfg blk =
  case kindBk blk of
    ListKB -> listItemSepFc cfg
    TableKB -> tableRowSepFc cfg
    TableRowKB -> tableCellSepFc cfg
    _ -> blockSepFc cfg

localChildSepForParent :: FlattenCfg -> Block b -> Text
localChildSepForParent cfg blk =
  case kindBk blk of
    TableRowKB -> tableCellSepFc cfg
    _ -> childBlockSepFc cfg

flattenFromNearestOrCurrent :: FlattenCfg -> (Block b -> Bool) -> BlockCtx d b -> Text
flattenFromNearestOrCurrent cfg predBlk ctx =
  case currentOrNearestPairBy predBlk ctx of
    Nothing -> flattenSubtreeText cfg ctx
    Just (path, blk) -> renderChunksText (flattenSubtreeChunksAt cfg path blk)

currentOrNearestPairBy :: (Block b -> Bool) -> BlockCtx d b -> Maybe (PathBlk, Block b)
currentOrNearestPairBy predBlk ctx =
  if predBlk ctx.blockBc then
    Just (ctx.pathBc, ctx.blockBc)
  else
    nearestAncestorPairBy predBlk ctx


nearestAncestorPairBy :: (Block b -> Bool) -> BlockCtx d b -> Maybe (PathBlk, Block b)
nearestAncestorPairBy predBlk ctx =
  let
    -- TODO: review if this logic makes sense:
    matches = [(frame.pathAf, ctx.blockBc) | predBlk ctx.blockBc, frame <- ancestorsBc ctx]
  in
  listToMaybe (sortOn (Down . pathDepth . fst) matches)


headingPairsFromCtx :: BlockCtx d b -> [(PathBlk, Block b)]
headingPairsFromCtx ctx =
  let
    ancestorHeadingPairs = [
    -- TODO: review if this logic makes sense:
        (frame.pathAf, ctx.blockBc) | isHeadingBlock ctx.blockBc, frame <- ancestorsBc ctx
      ]
    currentHeadingPairs = if isHeadingBlock ctx.blockBc then
        [(ctx.pathBc, ctx.blockBc)]
      else
        []
  in
  sortOn (pathDepth . fst) (ancestorHeadingPairs <> currentHeadingPairs)


isHeadingBlock :: Block b -> Bool
isHeadingBlock blk = blk.kindBk == HeadingKB


isListItemBlock :: Block b -> Bool
isListItemBlock blk = blk.kindBk == ListItemKB


isMessageBlock :: Block b -> Bool
isMessageBlock blk = blk.kindBk == MessageKB


isTableBlock :: Block b -> Bool
isTableBlock blk = blk.kindBk == TableKB


listItemMarkerText :: Block b -> Maybe Text
listItemMarkerText blk =
  case semBk blk of
    Just (ListItemSB sem) -> nonEmptyText (rawMarkerLsi sem) <|> (renderListMarker <$> markerLsi sem)
    _ -> Nothing

tableCaptionText :: Block b -> Maybe Text
tableCaptionText blk =
  let semCap =
        case semBk blk of
          Just (TableSB sem) -> captionTbs sem
          _ -> Nothing
  in nonEmptyText semCap <|> nonEmptyText (captionAb (attribsBk blk))

figureCaptionText :: Block b -> Maybe Text
figureCaptionText blk =
  let semCap =
        case semBk blk of
          Just (FigureSB sem) -> captionFgs sem
          _ -> Nothing
  in nonEmptyText semCap <|> nonEmptyText (captionAb (attribsBk blk))

imageAltText :: Block b -> Maybe Text
imageAltText blk =
  case semBk blk of
    Just (ImageSB sem) -> nonEmptyText (altTextIms sem)
    _ -> Nothing

noteMetaText :: Block b -> Maybe Text
noteMetaText blk =
  case semBk blk of
    Just (NoteSB sem) ->
      Just (renderNoteMeta sem)
    _ ->
      case kindBk blk of
        NoteKB nk -> Just (renderNoteKind nk)
        _ -> Nothing

messageMetaText :: Block b -> Maybe Text
messageMetaText blk =
  case semBk blk of
    Just (MessageSB sem) ->
      renderMessageMeta sem
    _ ->
      Nothing

customMetaText :: Block b -> Maybe Text
customMetaText blk =
  case kindBk blk of
    CustomKB tag ->
      let fieldParts =
            case semBk blk of
              Just (CustomSB sem) ->
                [ key <> "=" <> val
                | (key, val) <- M.toAscList (fieldsCst sem)
                ]
              _ ->
                []
          parts = ("tag=" <> tag) : fieldParts
      in Just (T.intercalate "; " parts)
    _ ->
      Nothing

renderMessageMeta :: MessageSemantics -> Maybe Text
renderMessageMeta sem =
  let parts =
        [ "role=" <> txt | txt <- maybeToList (nonEmptyText (roleMsg sem)) ]
        <> [ "author=" <> txt | txt <- maybeToList (nonEmptyText (authorMsg sem)) ]
        <> [ "ts=" <> txt | txt <- maybeToList (nonEmptyText (tsMsg sem)) ]
  in case parts of
       [] -> Nothing
       _ -> Just (T.intercalate "; " parts)

renderNoteMeta :: NoteSemantics -> Text
renderNoteMeta sem =
  case noteIdNts sem of
    Nothing -> renderNoteKind (kindNts sem)
    Just noteId -> renderNoteKind (kindNts sem) <> " " <> T.pack (show noteId)

renderNoteKind :: NoteKind -> Text
renderNoteKind noteKind =
  case noteKind of
    FootnoteNK -> "footnote"
    EndnoteNK -> "endnote"

renderListMarker :: ListMarker -> Text
renderListMarker marker =
  case marker of
    MarkerLabelled lbl -> rawLbl lbl
    MarkerGraphic gb -> renderGraphicBullet gb
    MarkerNativeOrdered n -> T.pack (show n) <> "."
    MarkerNativeBullet -> "•"
    MarkerNativeDefinition -> ":"

renderGraphicBullet :: GraphicBullet -> Text
renderGraphicBullet gb =
  case gb of
    HyphenBulletGB -> "-"
    AsteriskBulletGB -> "*"
    SolidBulletGB -> "•"
    WhiteBulletGB -> "◦"
    SquareBulletGB -> "▪"
    TriangularBulletGB -> "‣"

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText maybeTxt =
  case maybeTxt of
    Nothing -> Nothing
    Just txt ->
      if T.null txt
        then Nothing
        else Just txt

maybeToList :: Maybe a -> [a]
maybeToList maybeVal =
  case maybeVal of
    Nothing -> []
    Just val -> [val]