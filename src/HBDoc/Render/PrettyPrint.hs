{-# LANGUAGE LambdaCase #-}

module HBDoc.Render.PrettyPrint
  ( prettyHBDocText
  , prettyBlocksText
  , prettyBlockText
  , prettyParseResultText
  , renderFileReport
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import HBDoc.Core.Attributes
import HBDoc.Core.BlockKind
import HBDoc.Core.ListSemantics
import HBDoc.Core.Provenance
import HBDoc.Core.Semantics
import HBDoc.Core.Types

prettyHBDocText :: HBDoc docSpec blkSpec -> T.Text
prettyHBDocText doc =
  T.unlines (prettyHBDocLines doc)

prettyBlocksText :: [Block spec] -> T.Text
prettyBlocksText blocksDoc =
  T.unlines (concatMap (prettyBlockLines 0) blocksDoc)

prettyBlockText :: Block spec -> T.Text
prettyBlockText =
  T.unlines . prettyBlockLines 0

prettyParseResultText :: Either String (HBDoc docSpec blkSpec) -> T.Text
prettyParseResultText = \case
  Left err ->
    T.unlines
      [ "PARSE-ERROR"
      , indentText 1 (T.pack err)
      ]
  Right doc ->
    prettyHBDocText doc

renderFileReport :: FilePath -> Either String (HBDoc docSpec blkSpec) -> T.Text
renderFileReport path parseRes =
  T.unlines
    [ "================================================================================"
    , "FILE " <> T.pack (show path)
    , "================================================================================"
    , prettyParseResultText parseRes
    ]

prettyHBDocLines :: HBDoc docSpec blkSpec -> [T.Text]
prettyHBDocLines doc =
  let headLine =
        "DOCUMENT"
        <> " title=" <> quoted doc.titleDc
        <> maybe "" (\fmt -> " format=" <> quoted fmt) doc.formatDc
        <> " meta=" <> tshow (Map.size doc.metaDefDc)

      metaLines =
        if Map.null doc.metaDefDc
          then []
          else
            nodeLine 1 "META"
              : [ nodeLine 2 (keyTxt <> " = " <> renderMetaValue val)
                | (keyTxt, val) <- Map.toAscList doc.metaDefDc
                ]

      rootLines =
        prettyBlockLines 1 doc.rootBlkDc
  in [headLine] <> metaLines <> rootLines

prettyBlockLines :: Int -> Block spec -> [T.Text]
prettyBlockLines depth blk =
  let headLine = nodeLine depth (renderBlockHead blk)
      detailLines =
        renderAttribsLines (depth + 1) blk.attribsBk
        <> renderProvenanceLines (depth + 1) blk.provenanceBk
      childLines =
        concatMap (prettyBlockLines (depth + 1)) blk.childrenBk
  in headLine : detailLines <> childLines

renderBlockHead :: Block spec -> T.Text
renderBlockHead blk =
  case (blk.kindBk, blk.semBk) of
    (ContainerKB, _) ->
      "CONTAINER"
      <> " children=" <> tshow (length blk.childrenBk)

    (ParagraphKB, _) ->
      "PARAGRAPH"
      <> maybe "" (\txt -> " text=" <> quoted txt) blk.contentBk

    (HeadingKB, Just (HeadingSB sem)) ->
      "HEADING"
      <> " level=" <> tshow sem.levelHdg
      <> maybe "" (\lbl -> " label=" <> renderListLabel lbl) sem.labelHdg
      <> " source=" <> renderHeadingSource sem.sourceHdg
      <> maybe "" (\txt -> " title=" <> quoted txt) blk.contentBk
      <> renderChildrenCount blk

    (HeadingKB, semMb) ->
      "HEADING"
      <> renderUnexpectedSem semMb
      <> maybe "" (\txt -> " title=" <> quoted txt) blk.contentBk
      <> renderChildrenCount blk

    (ListKB, Just (ListSB sem)) ->
      "LIST"
      <> " kind=" <> renderListKind sem.kindLs
      <> " source=" <> renderListSource sem.sourceLs
      <> maybe "" (\n -> " start=" <> tshow n) sem.startAtLs
      <> maybe "" (\b -> " tight=" <> renderBool b) sem.isTightLs
      <> maybe "" (\b -> " continues=" <> renderBool b) sem.continuesPrevLs
      <> maybe "" (\sid -> " series=" <> quoted sid) sem.seriesIdLs
      <> " items=" <> tshow (length blk.childrenBk)

    (ListKB, semMb) ->
      "LIST"
      <> renderUnexpectedSem semMb
      <> " items=" <> tshow (length blk.childrenBk)

    (ListItemKB, Just (ListItemSB sem)) ->
      "ITEM"
      <> maybe "" (\marker -> " marker=" <> renderListMarker marker) sem.markerLsi
      <> " markerSource=" <> renderMarkerSource sem.markerSourceLsi
      <> maybe "" (\pres -> " style=" <> renderItemPresentation pres) sem.presentationLsi
      <> maybe "" (\raw -> " raw=" <> quoted raw) sem.rawMarkerLsi
      <> " inferred=" <> renderBool sem.isInferredLsi
      <> maybe "" (\txt -> " text=" <> quoted txt) blk.contentBk
      <> renderChildrenCount blk

    (ListItemKB, semMb) ->
      "ITEM"
      <> renderUnexpectedSem semMb
      <> maybe "" (\txt -> " text=" <> quoted txt) blk.contentBk
      <> renderChildrenCount blk

    (QuoteKB, _) ->
      "QUOTE"
      <> renderChildrenCount blk

    (CodeKB, Just (CodeSB sem)) ->
      "CODE"
      <> maybe "" (\lang -> " language=" <> quoted lang) sem.languageCds
      <> maybe "" (\info -> " info=" <> quoted info) sem.infoStringCds
      <> maybe "" (\txt -> " text=" <> quotedMultiline txt) blk.contentBk

    (CodeKB, semMb) ->
      "CODE"
      <> renderUnexpectedSem semMb
      <> maybe "" (\txt -> " text=" <> quotedMultiline txt) blk.contentBk

    (TableKB, Just (TableSB sem)) ->
      "TABLE"
      <> maybe "" (\n -> " headerRows=" <> tshow n) sem.headerRowsTbs
      <> maybe "" (\capTxt -> " caption=" <> quoted capTxt) sem.captionTbs
      <> " rows=" <> tshow (length blk.childrenBk)

    (TableKB, semMb) ->
      "TABLE"
      <> renderUnexpectedSem semMb
      <> " rows=" <> tshow (length blk.childrenBk)

    (TableRowKB, _) ->
      "TABLE-ROW"
      <> " cells=" <> tshow (length blk.childrenBk)

    (TableCellKB, Just (TableCellSB sem)) ->
      "TABLE-CELL"
      <> maybe "" (\n -> " rowSpan=" <> tshow n) sem.rowSpanTcs
      <> maybe "" (\n -> " colSpan=" <> tshow n) sem.colSpanTcs
      <> maybe "" (\txt -> " text=" <> quoted txt) blk.contentBk
      <> renderChildrenCount blk

    (TableCellKB, semMb) ->
      "TABLE-CELL"
      <> renderUnexpectedSem semMb
      <> maybe "" (\txt -> " text=" <> quoted txt) blk.contentBk
      <> renderChildrenCount blk

    (FigureKB, Just (FigureSB sem)) ->
      "FIGURE"
      <> maybe "" (\txt -> " caption=" <> quoted txt) sem.captionFgs
      <> renderChildrenCount blk

    (FigureKB, semMb) ->
      "FIGURE"
      <> renderUnexpectedSem semMb
      <> renderChildrenCount blk

    (ImageKB, Just (ImageSB sem)) ->
      "IMAGE"
      <> maybe "" (\txt -> " alt=" <> quoted txt) sem.altTextIms

    (ImageKB, semMb) ->
      "IMAGE"
      <> renderUnexpectedSem semMb

    (RuleKB, _) ->
      "RULE"

    (NoteKB noteKind, Just (NoteSB sem)) ->
      "NOTE"
      <> " kind=" <> renderNoteKind noteKind
      <> maybe "" (\n -> " id=" <> tshow n) sem.noteIdNts
      <> renderChildrenCount blk

    (NoteKB noteKind, semMb) ->
      "NOTE"
      <> " kind=" <> renderNoteKind noteKind
      <> renderUnexpectedSem semMb
      <> renderChildrenCount blk

    (ConversationKB, Just (ConversationSB sem)) ->
      "CONVERSATION"
      <> maybe "" (\tid -> " thread=" <> quoted tid) sem.threadIdCvs
      <> " messages=" <> tshow (length blk.childrenBk)

    (ConversationKB, semMb) ->
      "CONVERSATION"
      <> renderUnexpectedSem semMb
      <> " messages=" <> tshow (length blk.childrenBk)

    (MessageKB, Just (MessageSB sem)) ->
      "MESSAGE"
      <> maybe "" (\roleTxt -> " role=" <> quoted roleTxt) sem.roleMsg
      <> maybe "" (\authorTxt -> " author=" <> quoted authorTxt) sem.authorMsg
      <> maybe "" (\tsTxt -> " ts=" <> quoted tsTxt) sem.tsMsg
      <> maybe "" (\txt -> " text=" <> quoted txt) blk.contentBk
      <> renderChildrenCount blk

    (MessageKB, semMb) ->
      "MESSAGE"
      <> renderUnexpectedSem semMb
      <> maybe "" (\txt -> " text=" <> quoted txt) blk.contentBk
      <> renderChildrenCount blk

    (CustomKB tag, Just (CustomSB sem)) ->
      "CUSTOM"
      <> " tag=" <> quoted tag
      <> " semTag=" <> quoted sem.tagCst
      <> maybe "" (\txt -> " text=" <> quotedMultiline txt) blk.contentBk
      <> (if Map.null sem.fieldsCst
            then ""
            else " fields=" <> tshow (Map.size sem.fieldsCst))
      <> renderChildrenCount blk

    (CustomKB tag, semMb) ->
      "CUSTOM"
      <> " tag=" <> quoted tag
      <> renderUnexpectedSem semMb
      <> maybe "" (\txt -> " text=" <> quotedMultiline txt) blk.contentBk
      <> renderChildrenCount blk

renderChildrenCount :: Block spec -> T.Text
renderChildrenCount blk =
  case length blk.childrenBk of
    0 -> ""
    n -> " children=" <> tshow n

renderUnexpectedSem :: Maybe SemanticsBlk -> T.Text
renderUnexpectedSem = \case
  Nothing -> " sem=<none>"
  Just sem -> " sem=" <> renderSemanticsTag sem

renderAttribsLines :: Int -> AttributesBlk -> [T.Text]
renderAttribsLines depth attrs
  | isEmptyAttribs attrs = []
  | otherwise =
      concat
        [ maybe [] (\txt -> [nodeLine depth ("ATTR caption=" <> quoted txt)]) attrs.captionAb
        , maybe [] renderStyleAttrs attrs.styleAb
        , maybe [] renderNumberingAttrs attrs.numberingAb
        , maybe [] renderMediaAttrs attrs.mediaAb
        , maybe [] renderNoteRefAttrs attrs.noteRefAb
        , maybe [] renderHtmlAttrs attrs.htmlAb
        , if Map.null attrs.customAb
            then []
            else
              nodeLine depth ("ATTR custom=" <> tshow (Map.size attrs.customAb))
                : [ nodeLine (depth + 1) (keyTxt <> " = " <> quoted valTxt)
                  | (keyTxt, valTxt) <- Map.toAscList attrs.customAb
                  ]
        ]
  where
    renderStyleAttrs :: StyleAttrs -> [T.Text]
    renderStyleAttrs style =
      [ nodeLine depth
          ( "ATTR style"
          <> maybe "" (\txt -> " name=" <> quoted txt) style.nameSA
          <> maybe "" (\txt -> " id=" <> quoted txt) style.idSA
          )
      ]

    renderNumberingAttrs :: NumberingAttrs -> [T.Text]
    renderNumberingAttrs numbering =
      [ nodeLine depth
          ( "ATTR numbering"
          <> maybe "" (\n -> " numId=" <> tshow n) numbering.numIdNA
          <> maybe "" (\n -> " ilvl=" <> tshow n) numbering.ilvlNA
          <> maybe "" (\txt -> " numFmt=" <> quoted txt) numbering.numFmtNA
          <> maybe "" (\txt -> " lvlText=" <> quoted txt) numbering.lvlTextNA
          <> maybe "" (\n -> " startAt=" <> tshow n) numbering.startAtNA
          )
      ]

    renderMediaAttrs :: MediaAttrs -> [T.Text]
    renderMediaAttrs media =
      [ nodeLine depth
          ( "ATTR media"
          <> maybe "" (\txt -> " rid=" <> quoted txt) media.imageRidMa
          <> maybe "" (\txt -> " name=" <> quoted txt) media.imageNameMa
          <> maybe "" (\txt -> " key=" <> quoted txt) media.imageKeyMa
          <> maybe "" (\n -> " widthEmu=" <> tshow n) media.widthEmuMa
          <> maybe "" (\n -> " heightEmu=" <> tshow n) media.heightEmuMa
          )
      ]

    renderNoteRefAttrs :: NoteRefAttrs -> [T.Text]
    renderNoteRefAttrs noteRef =
      [ nodeLine depth
          ( "ATTR note-ref"
          <> maybe "" (\n -> " id=" <> tshow n) noteRef.noteIdNra
          <> maybe "" (\kind -> " kind=" <> renderNoteKind kind) noteRef.noteKindNra
          )
      ]

    renderHtmlAttrs :: HtmlAttrs -> [T.Text]
    renderHtmlAttrs html =
      let headLine =
            nodeLine depth
              ( "ATTR html"
              <> maybe "" (\txt -> " id=" <> quoted txt) html.htmlIdHa
              <> (if null html.htmlClassesHa
                    then ""
                    else " classes=" <> quoted (T.intercalate " " html.htmlClassesHa))
              <> " attrs=" <> tshow (Map.size html.htmlAttrsHa)
              )
          attrLines =
            [ nodeLine (depth + 1) (keyTxt <> " = " <> quoted valTxt)
            | (keyTxt, valTxt) <- Map.toAscList html.htmlAttrsHa
            ]
      in headLine : attrLines

renderProvenanceLines :: Int -> Maybe ProvenanceBlk -> [T.Text]
renderProvenanceLines _ Nothing = []
renderProvenanceLines depth (Just prov) =
  [ nodeLine depth
      ( "PROVENANCE"
      <> " source=" <> renderStructureSource prov.sourcePB
      <> maybe "" (\txt -> " ref=" <> quoted txt) prov.sourceRefPB
      <> maybe "" (\ik -> " inference=" <> renderInferenceKind ik) prov.inferencePB
      <> maybe "" (\c -> " confidence=" <> tshow c) prov.confidencePB
      )
  ]

isEmptyAttribs :: AttributesBlk -> Bool
isEmptyAttribs attrs =
  attrs.captionAb == Nothing
    && attrs.styleAb == Nothing
    && attrs.numberingAb == Nothing
    && attrs.mediaAb == Nothing
    && attrs.noteRefAb == Nothing
    && attrs.htmlAb == Nothing
    && Map.null attrs.customAb

nodeLine :: Int -> T.Text -> T.Text
nodeLine depth txt =
  indentPrefix depth <> "- " <> txt

indentPrefix :: Int -> T.Text
indentPrefix depth =
  T.replicate depth "  "

indentText :: Int -> T.Text -> T.Text
indentText depth txt =
  T.unlines
    [ indentPrefix depth <> lineTxt
    | lineTxt <- T.lines txt
    ]

renderListKind :: ListKind -> T.Text
renderListKind = \case
  OrderedLK -> "ordered"
  BulletLK -> "bullet"
  OutlineLK -> "outline"
  DefinitionLK -> "definition"

renderListSource :: ListSource -> T.Text
renderListSource = \case
  NativeListLS -> "native"
  ParsedTextListLS -> "parsed-text"
  ParsedHeadingListLS -> "parsed-heading"
  MixedListLS -> "mixed"

renderMarkerSource :: MarkerSource -> T.Text
renderMarkerSource = \case
  NativeMarkerMS -> "native"
  ParsedMarkerMS -> "parsed"
  GeneratedMarkerMS -> "generated"

renderHeadingSource :: HeadingSource -> T.Text
renderHeadingSource = \case
  NativeHeadingHS -> "native"
  PromotedListItemHS -> "promoted-list-item"
  ParsedLabelHeadingHS -> "parsed-label"

renderItemPresentation :: ItemPresentation -> T.Text
renderItemPresentation = \case
  HeadingPresentationIP n -> "Heading(" <> tshow n <> ")"

renderListMarker :: ListMarker -> T.Text
renderListMarker = \case
  MarkerLabelled lbl ->
    "Label(" <> lbl.rawLbl <> ")"
  MarkerGraphic bullet ->
    "Graphic(" <> renderGraphicBullet bullet <> ")"
  MarkerNativeOrdered n ->
    "Ordered(" <> tshow n <> ")"
  MarkerNativeBullet ->
    "Bullet"
  MarkerNativeDefinition ->
    "Definition"

renderListLabel :: ListLabel -> T.Text
renderListLabel lbl =
  let atomsTxt =
        T.intercalate "." (map renderLabelAtom (NE.toList lbl.atomsLbl))
      trailerTxt =
        maybe "" renderLabelTrailer lbl.trailerLbl
  in quoted (atomsTxt <> trailerTxt)

renderGraphicBullet :: GraphicBullet -> T.Text
renderGraphicBullet = \case
  HyphenBulletGB -> "-"
  AsteriskBulletGB -> "*"
  SolidBulletGB -> "•"
  WhiteBulletGB -> "◦"
  SquareBulletGB -> "▪"
  TriangularBulletGB -> "‣"

renderLabelAtom :: LabelAtom -> T.Text
renderLabelAtom = \case
  DecimalAtomLA n -> T.pack (show n)
  UpperAlphaAtomLA c -> T.singleton c
  LowerAlphaAtomLA c -> T.singleton c
  UpperRomanAtomLA n -> "R" <> tshow n
  LowerRomanAtomLA n -> "r" <> tshow n

renderLabelTrailer :: LabelTrailer -> T.Text
renderLabelTrailer = \case
  LabelDotLT -> "."
  LabelDashLT -> "-"
  LabelParenLT -> ")"
  LabelColonLT -> ":"

renderNoteKind :: NoteKind -> T.Text
renderNoteKind = \case
  FootnoteNK -> "footnote"
  EndnoteNK -> "endnote"

renderStructureSource :: StructureSource -> T.Text
renderStructureSource = \case
  PandocSS -> "pandoc"
  DocxXmlSS -> "docx-xml"
  MarkdownSS -> "markdown"
  NotionSS -> "notion"
  ConversationSS -> "conversation"
  StructDocSS -> "structdoc"
  ManualEditSS -> "manual-edit"
  OtherSS txt -> "other(" <> quoted txt <> ")"

renderInferenceKind :: InferenceKind -> T.Text
renderInferenceKind = \case
  ListReconstructionIK -> "list-reconstruction"
  HeadingPromotionIK -> "heading-promotion"
  HeadingLabelParseIK -> "heading-label-parse"
  ListContinuationIK -> "list-continuation"
  StructureNormalizationIK -> "structure-normalization"

renderSemanticsTag :: SemanticsBlk -> T.Text
renderSemanticsTag = \case
  HeadingSB _ -> "HeadingSB"
  ListSB _ -> "ListSB"
  ListItemSB _ -> "ListItemSB"
  CodeSB _ -> "CodeSB"
  TableSB _ -> "TableSB"
  TableCellSB _ -> "TableCellSB"
  FigureSB _ -> "FigureSB"
  ImageSB _ -> "ImageSB"
  NoteSB _ -> "NoteSB"
  ConversationSB _ -> "ConversationSB"
  MessageSB _ -> "MessageSB"
  CustomSB _ -> "CustomSB"

renderMetaValue :: MetaValue -> T.Text
renderMetaValue = \case
  MetaTextMV txt -> quoted txt
  MetaIntMV n -> tshow n
  MetaBoolMV b -> renderBool b
  MetaListMV vals ->
    "[" <> T.intercalate ", " (map renderMetaValue vals) <> "]"
  MetaMapMV mp ->
    "{"
      <> T.intercalate ", "
           [ keyTxt <> ": " <> renderMetaValue val
           | (keyTxt, val) <- Map.toAscList mp
           ]
      <> "}"

renderBool :: Bool -> T.Text
renderBool True = "true"
renderBool False = "false"

quoted :: T.Text -> T.Text
quoted txt =
  T.pack (show (T.unpack txt))

quotedMultiline :: T.Text -> T.Text
quotedMultiline =
  quoted . normalizeNewlines

normalizeNewlines :: T.Text -> T.Text
normalizeNewlines =
  T.replace "\r\n" "\n" . T.replace "\r" "\n"

tshow :: Show a => a -> T.Text
tshow =
  T.pack . show