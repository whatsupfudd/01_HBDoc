{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module HBDoc.Parse.Structured
  ( parse
  , parseDocxFile
  , parseMarkdownFile
  , parsePlainTextFile
  , parsePandocDoc
  ) where

import Control.Monad (guard)
import Data.Char
  ( isAsciiLower
  , isAsciiUpper
  , toLower
  , toUpper
  )

import Data.Default (def)
import Data.Foldable (foldl', toList)
import Data.Int (Int32)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import System.FilePath (takeBaseName, takeExtension)

import qualified Text.Megaparsec as MP
import Text.Megaparsec
  ( Parsec
  , eof
  , lookAhead
  , many
  , notFollowedBy
  , optional
  , runParser
  , sepBy1
  , some
  , takeRest
  , takeWhileP
  , try
  , (<|>)
  )
import Text.Megaparsec.Char
  ( char
  , digitChar
  , eol
  , hspace
  , hspace1
  )

import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Definition as P

import HBDoc.Core.BlockKind
import HBDoc.Core.Build
import HBDoc.Core.ListSemantics
import HBDoc.Core.Semantics
import HBDoc.Core.Types
import HBDoc.Core.Validate

type Parser = Parsec Void T.Text
type Block0 = Block ()
type Doc0 = HBDoc () ()

data ProtoBlock
  = ProtoReady Block0
  | ProtoDetected DetectedListItem
  deriving (Eq, Show)

data DetectedListItem = DetectedListItem
  { markerDet :: !ListMarker
  , bodyDet :: !T.Text
  , styleDet :: !(Maybe AtomStyle)
  , depthDet :: !Int
  }
  deriving (Eq, Show)

data AtomStyle
  = StyleUpperRoman
  | StyleUpperAlpha
  | StyleDecimal
  | StyleLowerAlpha
  | StyleLowerRoman
  deriving (Eq, Ord, Show)

data BuildState = BuildState
  { builderSt :: !OutlineBuilder
  , ordinaryProtoRevSt :: ![ProtoBlock]
  }
  deriving (Eq, Show)

data OutlineBuilder = OutlineBuilder
  { rootsRevBld :: ![RootEntry]
  , framesBld :: ![OutlineFrame]   -- shallow -> deep
  }
  deriving (Eq, Show)

data RootEntry
  = RootNormal Block0
  | RootOutlineItem Block0
  deriving (Eq, Show)

data OutlineFrame = OutlineFrame
  { atomFrm :: !LabelAtom
  , markerFrm :: !ListMarker
  , markerSourceFrm :: !MarkerSource
  , rawMarkerFrm :: !(Maybe T.Text)
  , textFrm :: !T.Text
  , presentationFrm :: !(Maybe ItemPresentation)
  , plainChildrenRevFrm :: ![Block0]
  , nestedItemsRevFrm :: ![Block0]
  }
  deriving (Eq, Show)

data HeadingSeed = HeadingSeed
  { pathHeading :: !(NonEmpty LabelAtom)
  , trailerHeading :: !(Maybe LabelTrailer)
  , titleHeading :: !T.Text
  , levelHeading :: !Int32
  }
  deriving (Eq, Show)

data RunBuilder = RunBuilder
  { rootsRevRunBld :: ![Block0]
  , framesRunBld :: ![RunFrame]
  }
  deriving (Eq, Show)

data RunFrame = RunFrame
  { keyRunFrm :: !(Maybe LabelAtom)
  , markerRunFrm :: !ListMarker
  , markerSourceRunFrm :: !MarkerSource
  , rawMarkerRunFrm :: !(Maybe T.Text)
  , textRunFrm :: !T.Text
  , nestedItemsRevRunFrm :: ![Block0]
  }
  deriving (Eq, Show)

parse :: FilePath -> IO (Either String Doc0)
parse path = do
  let ext = map toLower (takeExtension path)
  case ext of
    ".docx" -> parseDocxFile path
    ".md" -> parseMarkdownFile path
    ".txt" -> parsePlainTextFile path
    "" -> parsePlainTextFile path
    _ -> parsePlainTextFile path

parseMarkdownFile :: FilePath -> IO (Either String Doc0)
parseMarkdownFile path = do
  txt <- TIO.readFile path
  res <- Pandoc.runIO $ Pandoc.readMarkdown def [(path, txt)]
  pure $
    case res of
      Left err -> Left (show err)
      Right doc -> parsePandocDoc path "markdown" doc

parseDocxFile :: FilePath -> IO (Either String Doc0)
parseDocxFile path = do
  bytes <- LBS.readFile path
  res <- Pandoc.runIO $ Pandoc.readDocx def bytes
  pure $
    case res of
      Left err -> Left (show err)
      Right doc -> parsePandocDoc path "docx" doc

parsePlainTextFile :: FilePath -> IO (Either String Doc0)
parsePlainTextFile path = do
  txt <- TIO.readFile path
  pure $
    case runParser plainDocumentP path txt of
      Left err -> Left (MP.errorBundlePretty err)
      Right paragraphs ->
        finalizeDoc path "text" (finalizeProtoBlocks (map paragraphTextToProto paragraphs))

parsePandocDoc :: FilePath -> T.Text -> P.Pandoc -> Either String Doc0
parsePandocDoc path format doc =
  finalizeDoc path format (pandocToForest doc)

finalizeDoc :: FilePath -> T.Text -> [Block0] -> Either String Doc0
finalizeDoc path format blocksDoc =
  let doc =
        mkHBDocSimple
          ()
          (docTitleFromPath path)
          (Just format)
          (mkContainer0 blocksDoc)

      issues = validateHBDoc doc
  in if hasErrors issues
       then Left (T.unpack (T.intercalate "\n" (renderIssues issues)))
       else Right doc

docTitleFromPath :: FilePath -> T.Text
docTitleFromPath =
  T.pack . takeBaseName

pandocToForest :: P.Pandoc -> [Block0]
pandocToForest (P.Pandoc _ blocksDoc) =
  finalizeBuildState (foldl' stepPandocBlock emptyBuildState blocksDoc)

emptyBuildState :: BuildState
emptyBuildState =
  BuildState
    { builderSt =
        OutlineBuilder
          { rootsRevBld = []
          , framesBld = []
          }
    , ordinaryProtoRevSt = []
    }

stepPandocBlock :: BuildState -> P.Block -> BuildState
stepPandocBlock st blk =
  case extractHeadingSeed blk of
    Just seed ->
      let st1 = flushOrdinaryProto st
      in st1
          { builderSt = openHeadingSeed seed st1.builderSt
          }
    Nothing ->
      st
        { ordinaryProtoRevSt =
            reverse (pandocBlockToProtoNoHeading blk) <> st.ordinaryProtoRevSt
        }

finalizeBuildState :: BuildState -> [Block0]
finalizeBuildState st =
  finalizeOutlineBuilder (flushOrdinaryProto st).builderSt

flushOrdinaryProto :: BuildState -> BuildState
flushOrdinaryProto st =
  case reverse st.ordinaryProtoRevSt of
    [] -> st
    protoBlocks ->
      let ordinaryBlocks = finalizeProtoBlocks protoBlocks
      in st
          { builderSt = appendBlocksToBuilder ordinaryBlocks st.builderSt
          , ordinaryProtoRevSt = []
          }

appendBlocksToBuilder :: [Block0] -> OutlineBuilder -> OutlineBuilder
appendBlocksToBuilder [] builder = builder
appendBlocksToBuilder blocksDoc builder =
  case unsnoc builder.framesBld of
    Nothing ->
      builder
        { rootsRevBld = reverse (map RootNormal blocksDoc) <> builder.rootsRevBld
        }
    Just (parents, frame) ->
      let frame' =
            frame
              { plainChildrenRevFrm = reverse blocksDoc <> frame.plainChildrenRevFrm
              }
      in builder
          { framesBld = parents <> [frame']
          }

extractHeadingSeed :: P.Block -> Maybe HeadingSeed
extractHeadingSeed = \case
  P.Header levelBlk _ inlinesBlk ->
    let rawText = normalizeInlineText (inlineText inlinesBlk)
    in case runParser detectedListItemP "<heading>" rawText of
      Right item ->
        case item.markerDet of
          MarkerLabelled lbl ->
            Just
              HeadingSeed
                { pathHeading = lbl.atomsLbl
                , trailerHeading = lbl.trailerLbl
                , titleHeading = item.bodyDet
                , levelHeading = fromIntegral levelBlk
                }
          _ -> Nothing
      Left _ -> Nothing
  _ ->
    Nothing

openHeadingSeed :: HeadingSeed -> OutlineBuilder -> OutlineBuilder
openHeadingSeed seed builder =
  let atoms = toList seed.pathHeading
      stackAtoms = map (.atomFrm) builder.framesBld

      matchedPrefix0 = commonPrefixLength stackAtoms atoms
      matchedPrefix =
        if matchedPrefix0 == length atoms
          then max 0 (matchedPrefix0 - 1)
          else matchedPrefix0

      headingKeepDepth = headingParentDepth seed.levelHeading builder.framesBld
      keepDepth = max headingKeepDepth matchedPrefix

      builder1 = closeOutlineToDepth keepDepth builder
      suffix = drop matchedPrefix atoms
  in case unsnoc suffix of
      Nothing ->
        builder1
      Just (prefixAtoms, lastAtom) ->
        let builder2 =
              foldl'
                (\acc atom -> pushFrame (syntheticFrame atom) acc)
                builder1
                prefixAtoms

            terminal =
              terminalHeadingFrame
                lastAtom
                seed.trailerHeading
                seed.titleHeading
                seed.levelHeading
        in pushFrame terminal builder2

headingParentDepth :: Int32 -> [OutlineFrame] -> Int
headingParentDepth newLevel frames =
  maybe 0 fst
    (findLast
      (\(_, levelBlk) -> levelBlk < newLevel)
      [ (depth, levelBlk)
      | (depth, Just levelBlk) <- zip [1 :: Int ..] (map outlineFrameHeadingLevel frames)
      ])

outlineFrameHeadingLevel :: OutlineFrame -> Maybe Int32
outlineFrameHeadingLevel frame =
  case frame.presentationFrm of
    Just (HeadingPresentationIP levelBlk) -> Just levelBlk
    Nothing -> Nothing

commonPrefixLength :: Eq a => [a] -> [a] -> Int
commonPrefixLength xs ys =
  length (takeWhile id (zipWith (==) xs ys))

pushFrame :: OutlineFrame -> OutlineBuilder -> OutlineBuilder
pushFrame frame builder =
  builder
    { framesBld = builder.framesBld <> [frame]
    }

syntheticFrame :: LabelAtom -> OutlineFrame
syntheticFrame atom =
  OutlineFrame
    { atomFrm = atom
    , markerFrm = singleAtomMarker atom Nothing
    , markerSourceFrm = GeneratedMarkerMS
    , rawMarkerFrm = Nothing
    , textFrm = ""
    , presentationFrm = Nothing
    , plainChildrenRevFrm = []
    , nestedItemsRevFrm = []
    }

terminalHeadingFrame :: LabelAtom -> Maybe LabelTrailer -> T.Text -> Int32 -> OutlineFrame
terminalHeadingFrame atom trailer titleTxt levelBlk =
  OutlineFrame
    { atomFrm = atom
    , markerFrm = singleAtomMarker atom trailer
    , markerSourceFrm = ParsedMarkerMS
    , rawMarkerFrm = Just (renderLabelAtom atom <> maybe "" renderLabelTrailer trailer)
    , textFrm = titleTxt
    , presentationFrm = Just (HeadingPresentationIP levelBlk)
    , plainChildrenRevFrm = []
    , nestedItemsRevFrm = []
    }

closeOutlineToDepth :: Int -> OutlineBuilder -> OutlineBuilder
closeOutlineToDepth depthTarget builder
  | length builder.framesBld <= depthTarget = builder
  | otherwise = closeOutlineToDepth depthTarget (closeOneFrame builder)

closeOneFrame :: OutlineBuilder -> OutlineBuilder
closeOneFrame builder =
  case unsnoc builder.framesBld of
    Nothing ->
      builder
    Just (parents, frame) ->
      let closedBlock = closeFrameToBlock frame
      in case unsnoc parents of
          Nothing ->
            builder
              { framesBld = []
              , rootsRevBld = RootOutlineItem closedBlock : builder.rootsRevBld
              }
          Just (grandParents, parent) ->
            let parent' =
                  parent
                    { nestedItemsRevFrm = closedBlock : parent.nestedItemsRevFrm
                    }
            in builder
                { framesBld = grandParents <> [parent']
                }

closeFrameToBlock :: OutlineFrame -> Block0
closeFrameToBlock frame =
  let directChildren = reverse frame.plainChildrenRevFrm
      nestedItems = reverse frame.nestedItemsRevFrm
      nestedListChildren =
        case nestedItems of
          [] -> []
          xs ->
            [ mkList0
                (mkListSem OutlineLK ParsedHeadingListLS Nothing)
                xs
            ]
  in mkListItem0
      (nonEmptyTextMb frame.textFrm)
      ListItemSemantics
        { markerLsi = Just frame.markerFrm
        , markerSourceLsi = frame.markerSourceFrm
        , presentationLsi = frame.presentationFrm
        , rawMarkerLsi = frame.rawMarkerFrm
        , isInferredLsi = True
        }
      (directChildren <> nestedListChildren)

finalizeOutlineBuilder :: OutlineBuilder -> [Block0]
finalizeOutlineBuilder builder =
  let builder1 = closeOutlineToDepth 0 builder
  in groupRootEntries (reverse builder1.rootsRevBld)

groupRootEntries :: [RootEntry] -> [Block0]
groupRootEntries [] = []
groupRootEntries (entry : rest) =
  case entry of
    RootNormal blk ->
      blk : groupRootEntries rest
    RootOutlineItem blk ->
      let (itemsMore, rest1) = span isRootOutlineItem rest
          itemsAll = blk : [ item | RootOutlineItem item <- itemsMore ]
      in mkList0
          (mkListSem OutlineLK ParsedHeadingListLS Nothing)
          itemsAll
          : groupRootEntries rest1

isRootOutlineItem :: RootEntry -> Bool
isRootOutlineItem = \case
  RootOutlineItem _ -> True
  RootNormal _ -> False

pandocBlockToProtoNoHeading :: P.Block -> [ProtoBlock]
pandocBlockToProtoNoHeading = \case
  P.Plain inlinesBlk ->
    [ paragraphTextToProto (normalizeInlineText (inlineText inlinesBlk)) ]

  P.Para inlinesBlk ->
    [ paragraphTextToProto (normalizeInlineText (inlineText inlinesBlk)) ]

  P.Header levelBlk _ inlinesBlk ->
    let rawText = normalizeInlineText (inlineText inlinesBlk)
        (labelMb, titleText) = splitLeadingHeadingLabel rawText
        headingSem =
          HeadingSemantics
            { levelHdg = fromIntegral levelBlk
            , labelHdg = labelMb
            , sourceHdg =
                case labelMb of
                  Nothing -> NativeHeadingHS
                  Just _ -> ParsedLabelHeadingHS
            }
    in
      [ ProtoReady (mkHeading0 titleText headingSem []) ]

  P.BlockQuote childrenBlk ->
    [ ProtoReady
        (mkQuote0 (pandocToForest (P.Pandoc nullMeta childrenBlk)))
    ]

  P.OrderedList (startAt, _, _) itemsBlk ->
    [ ProtoReady
        (mkList0
          (mkListSem OrderedLK NativeListLS (Just (fromIntegral startAt)))
          (zipWith
            (nativeItemToBlock . MarkerNativeOrdered . fromIntegral)
            [startAt ..]
            itemsBlk))
    ]

  P.BulletList itemsBlk ->
    [ ProtoReady
        (mkList0
          (mkListSem BulletLK NativeListLS Nothing)
          (map (nativeItemToBlock MarkerNativeBullet) itemsBlk))
    ]

  P.DefinitionList entriesBlk ->
    [ ProtoReady
        (mkList0
          (mkListSem DefinitionLK NativeListLS Nothing)
          (map definitionEntryToBlock entriesBlk))
    ]

  P.CodeBlock _ codeBlk ->
    [ ProtoReady
        (mkCode0
          (Just codeBlk)
          CodeSemantics
            { languageCds = Nothing
            , infoStringCds = Nothing
            })
    ]

  P.HorizontalRule ->
    [ ProtoReady mkRule0 ]

  P.RawBlock _ rawBlk ->
    [ ProtoReady
        (mkCustom0
          "raw-block"
          (Just rawBlk)
          (Just (CustomSemantics "raw-block" mempty))
          [])
    ]

  P.Div _ childrenBlk ->
    [ ProtoReady
        (mkCustom0
          "div"
          Nothing
          (Just (CustomSemantics "div" mempty))
          (pandocToForest (P.Pandoc nullMeta childrenBlk)))
    ]

  P.LineBlock linesBlk ->
    let txt =
          normalizeInlineText
            (T.intercalate "\n" (map inlineText linesBlk))
    in [ paragraphTextToProto txt ]

  _ ->
    [ ProtoReady
        (mkCustom0
          "other-pandoc-block"
          Nothing
          (Just (CustomSemantics "other-pandoc-block" mempty))
          [])
    ]

nativeItemToBlock :: ListMarker -> [P.Block] -> Block0
nativeItemToBlock marker bodyBlk =
  promoteLeadParagraph marker (pandocToForest (P.Pandoc nullMeta bodyBlk))

definitionEntryToBlock :: ([P.Inline], [[P.Block]]) -> Block0
definitionEntryToBlock (termBlk, defsBlk) =
  let bodyChildren = concatMap (pandocToForest . P.Pandoc nullMeta) defsBlk
  in mkListItem0
      (nonEmptyTextMb (normalizeInlineText (inlineText termBlk)))
      ListItemSemantics
        { markerLsi = Just MarkerNativeDefinition
        , markerSourceLsi = NativeMarkerMS
        , presentationLsi = Nothing
        , rawMarkerLsi = Nothing
        , isInferredLsi = False
        }
      bodyChildren

promoteLeadParagraph :: ListMarker -> [Block0] -> Block0
promoteLeadParagraph marker bodyBlk =
  case bodyBlk of
    blk : rest
      | isPlainParagraph blk ->
          mkListItem0
            blk.contentBk
            (nativeItemSem marker)
            rest
    _ ->
      mkListItem0
        Nothing
        (nativeItemSem marker)
        bodyBlk

nativeItemSem :: ListMarker -> ListItemSemantics
nativeItemSem marker =
  ListItemSemantics
    { markerLsi = Just marker
    , markerSourceLsi = NativeMarkerMS
    , presentationLsi = Nothing
    , rawMarkerLsi = Nothing
    , isInferredLsi = False
    }

isPlainParagraph :: Block0 -> Bool
isPlainParagraph blk =
  blk.kindBk == ParagraphKB && blk.semBk == Nothing

singleAtomMarker :: LabelAtom -> Maybe LabelTrailer -> ListMarker
singleAtomMarker atom trailer =
  MarkerLabelled (singleAtomLabel atom trailer)

singleAtomLabel :: LabelAtom -> Maybe LabelTrailer -> ListLabel
singleAtomLabel atom trailer =
  let raw = renderLabelAtom atom <> maybe "" renderLabelTrailer trailer
  in ListLabel
      { atomsLbl = atom :| []
      , trailerLbl = trailer
      , rawLbl = raw
      }

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = do
  (ys, z) <- unsnoc xs
  pure (x : ys, z)

nullMeta :: P.Meta
nullMeta = P.nullMeta

finalizeProtoBlocks :: [ProtoBlock] -> [Block0]
finalizeProtoBlocks =
  sectionizeBlocks . groupDetectedLists

groupDetectedLists :: [ProtoBlock] -> [Block0]
groupDetectedLists [] = []
groupDetectedLists (ProtoReady blk : rest) =
  blk : groupDetectedLists rest
groupDetectedLists xs@(ProtoDetected _ : _) =
  let (runItems, rest) = span isDetected xs
      detected = [ item | ProtoDetected item <- runItems ]
  in detectedRunToBlock detected : groupDetectedLists rest

isDetected :: ProtoBlock -> Bool
isDetected = \case
  ProtoDetected _ -> True
  ProtoReady _ -> False

detectedRunToBlock :: [DetectedListItem] -> Block0
detectedRunToBlock itemsDet =
  let withDepths = normalizeDepths (inferOutlineDepths itemsDet)
      itemsNorm =
        [ item { depthDet = depth }
        | (item, depth) <- withDepths
        ]
      itemsBlk = buildDetectedRunForest itemsNorm
      kind =
        if all isGraphicDetected itemsNorm
          then BulletLK
          else OutlineLK
  in mkList0
      (mkListSem kind ParsedTextListLS Nothing)
      itemsBlk

emptyRunBuilder :: RunBuilder
emptyRunBuilder =
  RunBuilder
    { rootsRevRunBld = []
    , framesRunBld = []
    }

buildDetectedRunForest :: [DetectedListItem] -> [Block0]
buildDetectedRunForest itemsDet =
  finalizeRunBuilder (foldl' stepRunBuilder emptyRunBuilder itemsDet)

stepRunBuilder :: RunBuilder -> DetectedListItem -> RunBuilder
stepRunBuilder builder item =
  case explicitAtomPath item of
    Just atoms ->
      openExplicitDetected item atoms builder
    Nothing ->
      pushImplicitDetected item builder

explicitAtomPath :: DetectedListItem -> Maybe [LabelAtom]
explicitAtomPath item =
  case item.markerDet of
    MarkerLabelled lbl ->
      let atoms = toList lbl.atomsLbl
      in if length atoms > 1
           then Just atoms
           else Nothing
    _ ->
      Nothing

openExplicitDetected :: DetectedListItem -> [LabelAtom] -> RunBuilder -> RunBuilder
openExplicitDetected item atoms builder =
  let currentKeys = map (.keyRunFrm) builder.framesRunBld
      targetKeys = map Just atoms
      common0 = commonPrefixLength currentKeys targetKeys
      common =
        if common0 == length atoms
          then max 0 (common0 - 1)
          else common0
      builder1 = closeRunToDepth common builder
      suffix = drop common atoms
  in case unsnoc suffix of
      Nothing ->
        builder1
      Just (prefixAtoms, lastAtom) ->
        let builder2 =
              foldl'
                (\acc atom -> pushRunFrame (syntheticRunFrame atom) acc)
                builder1
                prefixAtoms
            terminal =
              RunFrame
                { keyRunFrm = Just lastAtom
                , markerRunFrm = detectedTerminalMarker item lastAtom
                , markerSourceRunFrm = ParsedMarkerMS
                , rawMarkerRunFrm = rawMarkerDet item
                , textRunFrm = item.bodyDet
                , nestedItemsRevRunFrm = []
                }
        in pushRunFrame terminal builder2

pushImplicitDetected :: DetectedListItem -> RunBuilder -> RunBuilder
pushImplicitDetected item builder =
  let targetDepth = max 1 item.depthDet
      builder1 = closeRunToDepth (targetDepth - 1) builder
      keyMb =
        case item.markerDet of
          MarkerLabelled lbl ->
            case toList lbl.atomsLbl of
              [atom] -> Just atom
              _ -> Nothing
          _ ->
            Nothing
      frame =
        RunFrame
          { keyRunFrm = keyMb
          , markerRunFrm = item.markerDet
          , markerSourceRunFrm = ParsedMarkerMS
          , rawMarkerRunFrm = rawMarkerDet item
          , textRunFrm = item.bodyDet
          , nestedItemsRevRunFrm = []
          }
  in pushRunFrame frame builder1

syntheticRunFrame :: LabelAtom -> RunFrame
syntheticRunFrame atom =
  RunFrame
    { keyRunFrm = Just atom
    , markerRunFrm = singleAtomMarker atom Nothing
    , markerSourceRunFrm = GeneratedMarkerMS
    , rawMarkerRunFrm = Nothing
    , textRunFrm = ""
    , nestedItemsRevRunFrm = []
    }

detectedTerminalMarker :: DetectedListItem -> LabelAtom -> ListMarker
detectedTerminalMarker item atom =
  case item.markerDet of
    MarkerLabelled lbl ->
      singleAtomMarker atom lbl.trailerLbl
    _ ->
      item.markerDet

pushRunFrame :: RunFrame -> RunBuilder -> RunBuilder
pushRunFrame frame builder =
  builder
    { framesRunBld = builder.framesRunBld <> [frame]
    }

closeRunToDepth :: Int -> RunBuilder -> RunBuilder
closeRunToDepth depthTarget builder
  | length builder.framesRunBld <= depthTarget = builder
  | otherwise = closeRunToDepth depthTarget (closeOneRunFrame builder)

closeOneRunFrame :: RunBuilder -> RunBuilder
closeOneRunFrame builder =
  case unsnoc builder.framesRunBld of
    Nothing ->
      builder
    Just (parents, frame) ->
      let closedBlock = closeRunFrameToBlock frame
      in case unsnoc parents of
          Nothing ->
            builder
              { framesRunBld = []
              , rootsRevRunBld = closedBlock : builder.rootsRevRunBld
              }
          Just (grandParents, parent) ->
            let parent' =
                  parent
                    { nestedItemsRevRunFrm = closedBlock : parent.nestedItemsRevRunFrm
                    }
            in builder
                { framesRunBld = grandParents <> [parent']
                }

closeRunFrameToBlock :: RunFrame -> Block0
closeRunFrameToBlock frame =
  let nestedItems = reverse frame.nestedItemsRevRunFrm
      nestedChildren =
        case nestedItems of
          [] -> []
          xs ->
            [ mkList0
                (mkListSem OutlineLK ParsedTextListLS Nothing)
                xs
            ]
  in mkListItem0
      (nonEmptyTextMb frame.textRunFrm)
      ListItemSemantics
        { markerLsi = Just frame.markerRunFrm
        , markerSourceLsi = frame.markerSourceRunFrm
        , presentationLsi = Nothing
        , rawMarkerLsi = frame.rawMarkerRunFrm
        , isInferredLsi = True
        }
      nestedChildren

finalizeRunBuilder :: RunBuilder -> [Block0]
finalizeRunBuilder builder =
  let builder1 = closeRunToDepth 0 builder
  in reverse builder1.rootsRevRunBld

isGraphicDetected :: DetectedListItem -> Bool
isGraphicDetected item =
  case item.markerDet of
    MarkerGraphic _ -> True
    _ -> False

inferOutlineDepths :: [DetectedListItem] -> [(DetectedListItem, Int)]
inferOutlineDepths =
  snd . foldl' step ([], [])
  where
    step ::
      ([(Int, AtomStyle)], [(DetectedListItem, Int)]) ->
      DetectedListItem ->
      ([(Int, AtomStyle)], [(DetectedListItem, Int)])
    step (stack, acc) item =
      let (depth, styleMb) = inferItemDepth stack item
          stack' =
            case styleMb of
              Nothing -> stack
              Just style -> updateStyleStack stack depth style
          item' = item { styleDet = styleMb, depthDet = depth }
      in (stack', acc <> [(item', depth)])

inferItemDepth :: [(Int, AtomStyle)] -> DetectedListItem -> (Int, Maybe AtomStyle)
inferItemDepth stack item =
  case item.markerDet of
    MarkerGraphic _ ->
      (1, Nothing)

    MarkerLabelled lbl ->
      let atoms = NE.toList lbl.atomsLbl
      in case atoms of
        [] ->
          (1, Nothing)

        [atom] ->
          let styleMb = atomStyle atom
              depth =
                case styleMb of
                  Nothing -> 1
                  Just style -> inferSingleAtomDepth stack style
          in (depth, styleMb)

        _ ->
          let depth = length atoms
              styleMb = atomStyle (last atoms)
          in (depth, styleMb)

    _ ->
      (1, Nothing)

inferSingleAtomDepth :: [(Int, AtomStyle)] -> AtomStyle -> Int
inferSingleAtomDepth [] _ = 1
inferSingleAtomDepth stack style =
  case reverse stack of
    [] ->
      1
    (depthTop, styleTop) : _
      | style == styleTop -> depthTop
      | style > styleTop -> depthTop + 1
      | otherwise -> maybe 1 id (lookupLastStyleDepth stack style)

lookupLastStyleDepth :: [(Int, AtomStyle)] -> AtomStyle -> Maybe Int
lookupLastStyleDepth stack style =
  fmap fst (findLast (\(_, style0) -> style0 == style) stack)

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast predicate =
  foldl'
    (\acc x -> if predicate x then Just x else acc)
    Nothing

updateStyleStack :: [(Int, AtomStyle)] -> Int -> AtomStyle -> [(Int, AtomStyle)]
updateStyleStack stack depth style =
  let shallower = filter (\(depth0, _) -> depth0 < depth) stack
  in shallower <> [(depth, style)]

normalizeDepths :: [(DetectedListItem, Int)] -> [(DetectedListItem, Int)]
normalizeDepths =
  snd . foldl' step (0 :: Int, [])
  where
    step ::
      (Int, [(DetectedListItem, Int)]) ->
      (DetectedListItem, Int) ->
      (Int, [(DetectedListItem, Int)])
    step (prevDepth, acc) (item, depth) =
      let depth' =
            if prevDepth == 0
              then max 1 depth
              else max 1 (min depth (prevDepth + 1))
      in (depth', acc <> [(item, depth')])

sectionizeBlocks :: [Block0] -> [Block0]
sectionizeBlocks xs =
  fst (gatherSections 0 xs)

gatherSections :: Int32 -> [Block0] -> ([Block0], [Block0])
gatherSections _ [] = ([], [])
gatherSections parentLevel (blk : rest) =
  case headingLevelOf blk of
    Just levelBlk ->
      if levelBlk <= parentLevel
        then ([], blk : rest)
        else
          let (children, rest1) = gatherSections levelBlk rest
              blk' = blk { childrenBk = blk.childrenBk <> children }
              (siblings, rest2) = gatherSections parentLevel rest1
          in (blk' : siblings, rest2)
    Nothing ->
      let (siblings, rest1) = gatherSections parentLevel rest
      in (blk : siblings, rest1)

headingLevelOf :: Block0 -> Maybe Int32
headingLevelOf blk =
  case blk.semBk of
    Just (HeadingSB sem)
      | blk.kindBk == HeadingKB -> Just sem.levelHdg
    _ ->
      Nothing

paragraphTextToProto :: T.Text -> ProtoBlock
paragraphTextToProto txt =
  let txt' = normalizeInlineText txt
  in case runParser detectedListItemP "<paragraph>" txt' of
    Right item ->
      ProtoDetected item
    Left _ ->
      ProtoReady (mkParagraph0 (nonEmptyTextMb txt'))

splitLeadingHeadingLabel :: T.Text -> (Maybe ListLabel, T.Text)
splitLeadingHeadingLabel txt =
  case runParser detectedListItemP "<heading>" txt of
    Right item ->
      case item.markerDet of
        MarkerLabelled lbl -> (Just lbl, item.bodyDet)
        _ -> (Nothing, txt)
    Left _ ->
      (Nothing, txt)

normalizeInlineText :: T.Text -> T.Text
normalizeInlineText =
  T.unwords . T.words . T.replace "\160" " " . T.strip

inlineText :: [P.Inline] -> T.Text
inlineText =
  T.concat . map inlineText1

inlineText1 :: P.Inline -> T.Text
inlineText1 = \case
  P.Str txt -> txt
  P.Space -> " "
  P.SoftBreak -> " "
  P.LineBreak -> "\n"
  P.Emph xs -> inlineText xs
  P.Strong xs -> inlineText xs
  P.Strikeout xs -> inlineText xs
  P.Superscript xs -> inlineText xs
  P.Subscript xs -> inlineText xs
  P.SmallCaps xs -> inlineText xs
  P.Quoted _ xs -> inlineText xs
  P.Cite _ xs -> inlineText xs
  P.Code _ txt -> txt
  P.Math _ txt -> txt
  P.RawInline _ txt -> txt
  P.Link _ xs _ -> inlineText xs
  P.Image _ xs _ -> inlineText xs
  P.Note blocksBlk ->
    "[" <> T.intercalate " " (map blockTextApprox blocksBlk) <> "]"
  P.Span _ xs -> inlineText xs

blockTextApprox :: P.Block -> T.Text
blockTextApprox = \case
  P.Plain xs -> inlineText xs
  P.Para xs -> inlineText xs
  P.Header _ _ xs -> inlineText xs
  P.CodeBlock _ txt -> txt
  P.BlockQuote xs -> T.intercalate " " (map blockTextApprox xs)
  P.OrderedList _ itemsBlk ->
    T.intercalate " " (map (T.intercalate " " . map blockTextApprox) itemsBlk)
  P.BulletList itemsBlk ->
    T.intercalate " " (map (T.intercalate " " . map blockTextApprox) itemsBlk)
  P.DefinitionList entriesBlk ->
    T.intercalate " "
      [ inlineText termBlk <> " " <> T.intercalate " " (concatMap (map blockTextApprox) defsBlk)
      | (termBlk, defsBlk) <- entriesBlk
      ]
  P.RawBlock _ txt -> txt
  P.Div _ xs -> T.intercalate " " (map blockTextApprox xs)
  P.LineBlock xss -> T.intercalate "\n" (map inlineText xss)
  _ -> ""

plainDocumentP :: Parser [T.Text]
plainDocumentP = do
  many blankLineP
  paras <- paragraphP `MP.sepEndBy` some blankLineP
  many blankLineP
  eof
  pure (filter (not . T.null . T.strip) paras)

paragraphP :: Parser T.Text
paragraphP = do
  ls <- some nonBlankLineP
  pure (T.intercalate "\n" ls)

nonBlankLineP :: Parser T.Text
nonBlankLineP = do
  notFollowedBy (lookAhead blankLineP)
  txt <- takeWhileP (Just "non blank line") (/= '\n')
  optional eol
  pure txt

blankLineP :: Parser ()
blankLineP = try $ do
  hspace
  _ <- eol
  pure ()

detectedListItemP :: Parser DetectedListItem
detectedListItemP =
  try graphicItemP
    <|> try complexLabelledItemP
    <|> simpleLabelledItemP

graphicItemP :: Parser DetectedListItem
graphicItemP = do
  hspace
  bullet <- graphicBulletP
  hspace1
  body <- takeRest
  let bodyTxt = T.strip body
  guard (not (T.null bodyTxt))
  pure
    DetectedListItem
      { markerDet = MarkerGraphic bullet
      , bodyDet = bodyTxt
      , styleDet = Nothing
      , depthDet = 1
      }

simpleLabelledItemP :: Parser DetectedListItem
simpleLabelledItemP = do
  hspace
  atom <- simpleLabelAtomP
  hspace
  trailer <- labelTrailerP
  hspace1
  body <- takeRest
  let bodyTxt = T.strip body
      label = mkListLabel (atom :| []) (Just trailer)
  guard (not (T.null bodyTxt))
  pure
    DetectedListItem
      { markerDet = MarkerLabelled label
      , bodyDet = bodyTxt
      , styleDet = atomStyle atom
      , depthDet = 1
      }

complexLabelledItemP :: Parser DetectedListItem
complexLabelledItemP = do
  hspace
  atoms <- labelAtomP `sepBy1` char '.'
  guard (length atoms > 1)
  trailer <- optional labelTrailerP
  hspace1
  body <- takeRest
  let atomsNe =
        case NE.nonEmpty atoms of
          Just xs -> xs
          Nothing -> error "impossible: sepBy1 produced an empty list"
      bodyTxt = T.strip body
      label = mkListLabel atomsNe trailer
  guard (not (T.null bodyTxt))
  pure
    DetectedListItem
      { markerDet = MarkerLabelled label
      , bodyDet = bodyTxt
      , styleDet = atomStyle (last atoms)
      , depthDet = length atoms
      }

mkListLabel :: NonEmpty LabelAtom -> Maybe LabelTrailer -> ListLabel
mkListLabel atoms trailer =
  let raw =
        T.intercalate "." (map renderLabelAtom (toList atoms))
          <> maybe "" renderLabelTrailer trailer
  in ListLabel
      { atomsLbl = atoms
      , trailerLbl = trailer
      , rawLbl = raw
      }

labelAtomP :: Parser LabelAtom
labelAtomP =
  try decimalAtomP
    <|> try romanLongAtomP
    <|> alphaAtomP

simpleLabelAtomP :: Parser LabelAtom
simpleLabelAtomP =
  try decimalAtomP
    <|> try romanLongAtomP
    <|> alphaAtomP

decimalAtomP :: Parser LabelAtom
decimalAtomP = do
  digits <- some digitChar
  pure (DecimalAtomLA (read digits))

alphaAtomP :: Parser LabelAtom
alphaAtomP = do
  c <- MP.satisfy (\x -> isAsciiUpper x || isAsciiLower x)
  pure $
    if isAsciiUpper c
      then UpperAlphaAtomLA c
      else LowerAlphaAtomLA c

romanLongAtomP :: Parser LabelAtom
romanLongAtomP = do
  raw <- some (MP.satisfy isRomanChar)
  guard (length raw > 1)
  value <- maybe MP.empty pure (romanToInt raw)
  pure $
    if isAsciiUpper (head raw)
      then UpperRomanAtomLA (fromIntegral value)
      else LowerRomanAtomLA (fromIntegral value)

isRomanChar :: Char -> Bool
isRomanChar c =
  toUpper c `elem` ("IVXLCDM" :: String)

romanToInt :: String -> Maybe Int
romanToInt raw = do
  values <- traverse romanValue raw
  pure (collapseRoman values)

romanValue :: Char -> Maybe Int
romanValue c =
  case toUpper c of
    'I' -> Just 1
    'V' -> Just 5
    'X' -> Just 10
    'L' -> Just 50
    'C' -> Just 100
    'D' -> Just 500
    'M' -> Just 1000
    _ -> Nothing

collapseRoman :: [Int] -> Int
collapseRoman [] = 0
collapseRoman [x] = x
collapseRoman (x : y : xs)
  | x < y = (-x) + collapseRoman (y : xs)
  | otherwise = x + collapseRoman (y : xs)

labelTrailerP :: Parser LabelTrailer
labelTrailerP =
  LabelDotLT <$ char '.'
    <|> LabelDashLT <$ char '-'
    <|> LabelParenLT <$ char ')'
    <|> LabelColonLT <$ char ':'

graphicBulletP :: Parser GraphicBullet
graphicBulletP =
  HyphenBulletGB <$ char '-'
    <|> AsteriskBulletGB <$ char '*'
    <|> SolidBulletGB <$ char '•'
    <|> WhiteBulletGB <$ char '◦'
    <|> SquareBulletGB <$ char '▪'
    <|> TriangularBulletGB <$ char '‣'

atomStyle :: LabelAtom -> Maybe AtomStyle
atomStyle = \case
  DecimalAtomLA _ -> Just StyleDecimal
  UpperAlphaAtomLA _ -> Just StyleUpperAlpha
  LowerAlphaAtomLA _ -> Just StyleLowerAlpha
  UpperRomanAtomLA _ -> Just StyleUpperRoman
  LowerRomanAtomLA _ -> Just StyleLowerRoman

rawMarkerDet :: DetectedListItem -> Maybe T.Text
rawMarkerDet item =
  rawMarkerFromListMarker item.markerDet

rawMarkerFromListMarker :: ListMarker -> Maybe T.Text
rawMarkerFromListMarker = \case
  MarkerLabelled lbl -> Just lbl.rawLbl
  MarkerGraphic bullet -> Just (renderGraphicBullet bullet)
  MarkerNativeOrdered _ -> Nothing
  MarkerNativeBullet -> Nothing
  MarkerNativeDefinition -> Nothing

renderLabelAtom :: LabelAtom -> T.Text
renderLabelAtom = \case
  DecimalAtomLA n -> T.pack (show n)
  UpperAlphaAtomLA c -> T.singleton c
  LowerAlphaAtomLA c -> T.singleton c
  UpperRomanAtomLA n -> intToRoman True (fromIntegral n)
  LowerRomanAtomLA n -> intToRoman False (fromIntegral n)

renderLabelTrailer :: LabelTrailer -> T.Text
renderLabelTrailer = \case
  LabelDotLT -> "."
  LabelDashLT -> "-"
  LabelParenLT -> ")"
  LabelColonLT -> ":"

renderGraphicBullet :: GraphicBullet -> T.Text
renderGraphicBullet = \case
  HyphenBulletGB -> "-"
  AsteriskBulletGB -> "*"
  SolidBulletGB -> "•"
  WhiteBulletGB -> "◦"
  SquareBulletGB -> "▪"
  TriangularBulletGB -> "‣"

intToRoman :: Bool -> Int -> T.Text
intToRoman uppercase n =
  let raw = go n romanDigits
      txt = T.pack raw
  in if uppercase then txt else T.toLower txt
  where
    romanDigits :: [(Int, String)]
    romanDigits =
      [ (1000, "M")
      , (900, "CM")
      , (500, "D")
      , (400, "CD")
      , (100, "C")
      , (90, "XC")
      , (50, "L")
      , (40, "XL")
      , (10, "X")
      , (9, "IX")
      , (5, "V")
      , (4, "IV")
      , (1, "I")
      ]

    go :: Int -> [(Int, String)] -> String
    go 0 _ = ""
    go _ [] = ""
    go x ((value, glyph) : rest)
      | x >= value = glyph <> go (x - value) ((value, glyph) : rest)
      | otherwise = go x rest

mkListSem :: ListKind -> ListSource -> Maybe Int32 -> ListSemantics
mkListSem kind source startAt =
  ListSemantics
    { kindLs = kind
    , sourceLs = source
    , startAtLs = startAt
    , isTightLs = Nothing
    , continuesPrevLs = Nothing
    , seriesIdLs = Nothing
    }

nonEmptyTextMb :: T.Text -> Maybe T.Text
nonEmptyTextMb txt =
  let txt' = T.strip txt
  in if T.null txt' then Nothing else Just txt'