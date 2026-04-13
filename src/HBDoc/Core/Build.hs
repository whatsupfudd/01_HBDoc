{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Core.Build where

import Data.Map.Strict (Map)
import Data.Text (Text)

import HBDoc.Core.Attributes
import HBDoc.Core.BlockKind
import HBDoc.Core.Provenance
import HBDoc.Core.Semantics
import HBDoc.Core.ListSemantics
import HBDoc.Core.Types

-- | Canonical document constructor.
-- The caller provides the already-built root block.
mkHBDoc ::
  docSpec ->
  Text ->
  Maybe Text ->
  Map Text MetaValue ->
  Block blkSpec ->
  HBDoc docSpec blkSpec
mkHBDoc extra title format meta root =
  HBDoc
    { titleDc = title
    , formatDc = format
    , metaDefDc = meta
    , rootBlkDc = root
    , extraDc = extra
    }

-- | Canonical document constructor with empty metadata.
mkHBDocSimple ::
  docSpec ->
  Text ->
  Maybe Text ->
  Block blkSpec ->
  HBDoc docSpec blkSpec
mkHBDocSimple extra title format root =
  mkHBDoc extra title format mempty root

-- | Root/container block.
mkContainerBk :: blkSpec -> [Block blkSpec] -> Block blkSpec
mkContainerBk extra kids =
  baseBk extra ContainerKB Nothing Nothing kids

-- | Plain paragraph block.
mkParagraphBk :: blkSpec -> Maybe Text -> Block blkSpec
mkParagraphBk extra txt =
  baseBk extra ParagraphKB txt Nothing []

-- | Heading block.
-- The heading text goes in 'contentBk', and the heading semantics
-- carry the level/label/source.
mkHeadingBk ::
  blkSpec ->
  Text ->
  HeadingSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkHeadingBk extra txt sem kids =
  baseBk extra HeadingKB (Just txt) (Just (HeadingSB sem)) kids

-- | List container block.
mkListBk ::
  blkSpec ->
  ListSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkListBk extra sem kids =
  baseBk extra ListKB Nothing (Just (ListSB sem)) kids

-- | List item block.
-- The main visible text of the item goes in 'contentBk'.
-- Richer nested structure can go in 'childrenBk'.
mkListItemBk ::
  blkSpec ->
  Maybe Text ->
  ListItemSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkListItemBk extra txt sem kids =
  baseBk extra ListItemKB txt (Just (ListItemSB sem)) kids

-- | Quote block.
mkQuoteBk :: blkSpec -> [Block blkSpec] -> Block blkSpec
mkQuoteBk extra kids =
  baseBk extra QuoteKB Nothing Nothing kids

-- | Code block with typed semantics.
mkCodeBk ::
  blkSpec ->
  Maybe Text ->
  CodeSemantics ->
  Block blkSpec
mkCodeBk extra txt sem =
  baseBk extra CodeKB txt (Just (CodeSB sem)) []

-- | Table block.
mkTableBk ::
  blkSpec ->
  TableSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkTableBk extra sem kids =
  baseBk extra TableKB Nothing (Just (TableSB sem)) kids

-- | Table row block.
mkTableRowBk :: blkSpec -> [Block blkSpec] -> Block blkSpec
mkTableRowBk extra kids =
  baseBk extra TableRowKB Nothing Nothing kids

-- | Table cell block.
mkTableCellBk ::
  blkSpec ->
  Maybe Text ->
  TableCellSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkTableCellBk extra txt sem kids =
  baseBk extra TableCellKB txt (Just (TableCellSB sem)) kids

-- | Figure block.
mkFigureBk ::
  blkSpec ->
  FigureSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkFigureBk extra sem kids =
  baseBk extra FigureKB Nothing (Just (FigureSB sem)) kids

-- | Image block.
mkImageBk ::
  blkSpec ->
  ImageSemantics ->
  Block blkSpec
mkImageBk extra sem =
  baseBk extra ImageKB Nothing (Just (ImageSB sem)) []

-- | Horizontal rule block.
mkRuleBk :: blkSpec -> Block blkSpec
mkRuleBk extra =
  baseBk extra RuleKB Nothing Nothing []

-- | Note block.
mkNoteBk ::
  blkSpec ->
  NoteSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkNoteBk extra sem kids =
  baseBk extra (NoteKB sem.kindNts) Nothing (Just (NoteSB sem)) kids

-- | Conversation block.
mkConversationBk ::
  blkSpec ->
  ConversationSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkConversationBk extra sem kids =
  baseBk extra ConversationKB Nothing (Just (ConversationSB sem)) kids

-- | Message block.
mkMessageBk ::
  blkSpec ->
  Maybe Text ->
  MessageSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkMessageBk extra txt sem kids =
  baseBk extra MessageKB txt (Just (MessageSB sem)) kids

-- | Custom block with typed custom semantics.
mkCustomBk ::
  blkSpec ->
  Text ->
  Maybe Text ->
  Maybe CustomSemantics ->
  [Block blkSpec] ->
  Block blkSpec
mkCustomBk extra tag txt sem kids =
  baseBk extra (CustomKB tag) txt (CustomSB <$> sem) kids

-- | Attach non-semantic block attributes.
withAttribsBk :: AttributesBlk -> Block spec -> Block spec
withAttribsBk attribs blk =
  blk { attribsBk = attribs }

-- | Attach provenance.
withProvenanceBk :: ProvenanceBlk -> Block spec -> Block spec
withProvenanceBk provenance blk =
  blk { provenanceBk = Just provenance }

-- | Clear provenance.
withoutProvenanceBk :: Block spec -> Block spec
withoutProvenanceBk blk =
  blk { provenanceBk = Nothing }

-- | Replace children on an existing block.
withChildrenBk :: [Block spec] -> Block spec -> Block spec
withChildrenBk kids blk =
  blk { childrenBk = kids }

-- | Replace content on an existing block.
withContentBk :: Maybe Text -> Block spec -> Block spec
withContentBk txt blk =
  blk { contentBk = txt }

-- | Replace extra payload on an existing block.
withExtraBk :: spec -> Block spec -> Block spec
withExtraBk extra blk =
  blk { extraBk = extra }

-- | Helper for empty-spec documents and blocks.
--
-- Example:
--   mkParagraph0 (Just "Hello")
mkContainer0 :: [Block ()] -> Block ()
mkContainer0 = mkContainerBk ()

mkParagraph0 :: Maybe Text -> Block ()
mkParagraph0 = mkParagraphBk ()

mkHeading0 :: Text -> HeadingSemantics -> [Block ()] -> Block ()
mkHeading0 = mkHeadingBk ()

mkList0 :: ListSemantics -> [Block ()] -> Block ()
mkList0 = mkListBk ()

mkListItem0 :: Maybe Text -> ListItemSemantics -> [Block ()] -> Block ()
mkListItem0 = mkListItemBk ()

mkQuote0 :: [Block ()] -> Block ()
mkQuote0 = mkQuoteBk ()

mkCode0 :: Maybe Text -> CodeSemantics -> Block ()
mkCode0 = mkCodeBk ()

mkTable0 :: TableSemantics -> [Block ()] -> Block ()
mkTable0 = mkTableBk ()

mkTableRow0 :: [Block ()] -> Block ()
mkTableRow0 = mkTableRowBk ()

mkTableCell0 :: Maybe Text -> TableCellSemantics -> [Block ()] -> Block ()
mkTableCell0 = mkTableCellBk ()

mkFigure0 :: FigureSemantics -> [Block ()] -> Block ()
mkFigure0 = mkFigureBk ()

mkImage0 :: ImageSemantics -> Block ()
mkImage0 = mkImageBk ()

mkRule0 :: Block ()
mkRule0 = mkRuleBk ()

mkNote0 :: NoteSemantics -> [Block ()] -> Block ()
mkNote0 = mkNoteBk ()

mkConversation0 :: ConversationSemantics -> [Block ()] -> Block ()
mkConversation0 = mkConversationBk ()

mkMessage0 :: Maybe Text -> MessageSemantics -> [Block ()] -> Block ()
mkMessage0 = mkMessageBk ()

mkCustom0 :: Text -> Maybe Text -> Maybe CustomSemantics -> [Block ()] -> Block ()
mkCustom0 = mkCustomBk ()

mkHBDoc0 ::
  () ->
  Text ->
  Maybe Text ->
  Block () ->
  HBDoc () ()
mkHBDoc0 extra title format root =
  mkHBDocSimple extra title format root

-- Internal helper

baseBk ::
  blkSpec ->
  KindBlk ->
  Maybe Text ->
  Maybe SemanticsBlk ->
  [Block blkSpec] ->
  Block blkSpec
baseBk extra kind txt sem kids =
  Block
    { kindBk = kind
    , contentBk = txt
    , semBk = sem
    , attribsBk = emptyAttribs
    , provenanceBk = Nothing
    , childrenBk = kids
    , extraBk = extra
    }