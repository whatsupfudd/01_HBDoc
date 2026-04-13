{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Core.Validate where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import HBDoc.Core.BlockKind
import HBDoc.Core.Semantics
import HBDoc.Core.Types

data ValidationLevel =
    ErrorVL
  | WarningVL
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data ValidationIssue = ValidationIssue {
    levelVi :: !ValidationLevel
  , pathVi :: ![Int]
  , kindVi :: !(Maybe KindBlk)
  , msgVi :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

validateHBDoc :: HBDoc docSpec blkSpec -> [ValidationIssue]
validateHBDoc doc =
  let root = rootBlkDc doc
  in validateRootBlk root <> validateBlockTree root

isValidHBDoc :: HBDoc docSpec blkSpec -> Bool
isValidHBDoc doc =
  not (hasErrors (validateHBDoc doc))

validateBlockTree :: Block spec -> [ValidationIssue]
validateBlockTree =
  validateBlockAt []

hasErrors :: [ValidationIssue] -> Bool
hasErrors =
  any (\issue -> levelVi issue == ErrorVL)

renderIssue :: ValidationIssue -> Text
renderIssue issue =
  let levelTxt =
        case levelVi issue of
          ErrorVL -> "error"
          WarningVL -> "warning"

      pathTxt = renderPath (pathVi issue)

      kindTxt =
        case kindVi issue of
          Nothing -> ""
          Just kind -> " [" <> renderKind kind <> "]"
  in levelTxt <> " at " <> pathTxt <> kindTxt <> ": " <> msgVi issue

renderIssues :: [ValidationIssue] -> [Text]
renderIssues =
  map renderIssue

validateRootBlk :: Block spec -> [ValidationIssue]
validateRootBlk blk =
  let baseIssues =
        case kindBk blk of
          ContainerKB -> []
          otherKind ->
            [ mkError [] (Just otherKind)
                "Root block must have kind ContainerKB."
            ]

      contentIssues =
        case contentBk blk of
          Nothing -> []
          Just _ ->
            [ mkWarning [] (Just (kindBk blk))
                "Root container should not carry direct content."
            ]
  in baseIssues <> contentIssues

validateBlockAt :: [Int] -> Block spec -> [ValidationIssue]
validateBlockAt path blk =
  let selfIssues =
        validateSemanticsAt path blk
        <> validateStructureAt path blk
        <> validateContentAt path blk

      childIssues =
        concatMap
          (\(idx, child) -> validateBlockAt (path <> [idx]) child)
          (zip [0 :: Int ..] (childrenBk blk))
  in selfIssues <> childIssues

validateSemanticsAt :: [Int] -> Block spec -> [ValidationIssue]
validateSemanticsAt path blk =
  case kindBk blk of
    ContainerKB ->
      expectNoSem path blk "Container blocks must not carry block semantics."

    ParagraphKB ->
      expectNoSem path blk "Paragraph blocks must not carry block semantics."

    QuoteKB ->
      expectNoSem path blk "Quote blocks must not carry block semantics."

    TableRowKB ->
      expectNoSem path blk "Table-row blocks must not carry block semantics."

    RuleKB ->
      expectNoSem path blk "Rule blocks must not carry block semantics."

    HeadingKB ->
      expectSem path blk isHeadingSem
        "Heading blocks must carry Just (HeadingSB ...)."

    ListKB ->
      expectSem path blk isListSem
        "List blocks must carry Just (ListSB ...)."

    ListItemKB ->
      expectSem path blk isListItemSem
        "List-item blocks must carry Just (ListItemSB ...)."

    CodeKB ->
      expectSem path blk isCodeSem
        "Code blocks must carry Just (CodeSB ...)."

    TableKB ->
      expectSem path blk isTableSem
        "Table blocks must carry Just (TableSB ...)."

    TableCellKB ->
      expectSem path blk isTableCellSem
        "Table-cell blocks must carry Just (TableCellSB ...)."

    FigureKB ->
      expectSem path blk isFigureSem
        "Figure blocks must carry Just (FigureSB ...)."

    ImageKB ->
      expectSem path blk isImageSem
        "Image blocks must carry Just (ImageSB ...)."

    NoteKB noteKind ->
      case semBk blk of
        Just (NoteSB sem)
          | kindNts sem == noteKind -> []
          | otherwise ->
              [ mkError path (Just (kindBk blk))
                  "NoteKB kind and NoteSB semantics disagree on the note kind."
              ]
        Nothing ->
          [ mkError path (Just (kindBk blk))
              "Note blocks must carry Just (NoteSB ...)."
          ]
        Just otherSem ->
          [ mkError path (Just (kindBk blk))
              ("Note blocks must carry NoteSB semantics, found " <> renderSemantics otherSem <> ".")
          ]

    ConversationKB ->
      expectSem path blk isConversationSem
        "Conversation blocks must carry Just (ConversationSB ...)."

    MessageKB ->
      expectSem path blk isMessageSem
        "Message blocks must carry Just (MessageSB ...)."

    CustomKB tag ->
      case semBk blk of
        Nothing ->
          []
        Just (CustomSB sem)
          | tagCst sem == tag -> []
          | otherwise ->
              [ mkError path (Just (kindBk blk))
                  "CustomKB tag and CustomSB tag disagree."
              ]
        Just otherSem ->
          [ mkError path (Just (kindBk blk))
              ("CustomKB may only carry CustomSB semantics, found " <> renderSemantics otherSem <> ".")
          ]

validateStructureAt :: [Int] -> Block spec -> [ValidationIssue]
validateStructureAt path blk =
  case kindBk blk of
    ListKB ->
      validateChildKinds path blk isListItemKind
        "List blocks may only contain ListItemKB children."

    TableKB ->
      validateChildKinds path blk isTableRowKind
        "Table blocks may only contain TableRowKB children."

    TableRowKB ->
      validateChildKinds path blk isTableCellKind
        "Table-row blocks may only contain TableCellKB children."

    ConversationKB ->
      validateChildKinds path blk isMessageKind
        "Conversation blocks may only contain MessageKB children."

    ListItemKB ->
      [ mkError (path <> [idx]) (Just (kindBk child))
          "List-item blocks must not contain direct ListItemKB children; nested items must be wrapped in a ListKB container."
      | (idx, child) <- zip [0 :: Int ..] (childrenBk blk)
      , kindBk child == ListItemKB
      ]

    ParagraphKB ->
      requireNoChildren path blk
        "Paragraph blocks must not contain child blocks."

    CodeKB ->
      requireNoChildren path blk
        "Code blocks must not contain child blocks."

    ImageKB ->
      requireNoChildren path blk
        "Image blocks must not contain child blocks."

    RuleKB ->
      requireNoChildren path blk
        "Rule blocks must not contain child blocks."

    _ ->
      []

validateContentAt :: [Int] -> Block spec -> [ValidationIssue]
validateContentAt path blk =
  case kindBk blk of
    ContainerKB ->
      requireNoContentWarn path blk
        "Container blocks should not carry direct content."

    ListKB ->
      requireNoContentWarn path blk
        "List blocks should not carry direct content."

    QuoteKB ->
      requireNoContentWarn path blk
        "Quote blocks should generally carry quoted content in child blocks rather than in contentBk."

    TableKB ->
      requireNoContentWarn path blk
        "Table blocks should not carry direct content."

    TableRowKB ->
      requireNoContentWarn path blk
        "Table-row blocks should not carry direct content."

    FigureKB ->
      requireNoContentWarn path blk
        "Figure blocks should generally carry content through semantics, attributes, or child blocks rather than in contentBk."

    ImageKB ->
      requireNoContentWarn path blk
        "Image blocks should not carry direct content."

    RuleKB ->
      requireNoContentWarn path blk
        "Rule blocks should not carry direct content."

    ConversationKB ->
      requireNoContentWarn path blk
        "Conversation blocks should not carry direct content."

    HeadingKB ->
      case contentBk blk of
        Nothing ->
          [ mkWarning path (Just HeadingKB)
              "Heading blocks usually should carry their visible title in contentBk."
          ]
        Just _ ->
          []

    ListItemKB ->
      if hasVisiblePayload blk
        then []
        else
          [ mkWarning path (Just ListItemKB)
              "List-item block has neither direct content nor child blocks."
          ]

    MessageKB ->
      if hasVisiblePayload blk
        then []
        else
          [ mkWarning path (Just MessageKB)
              "Message block has neither direct content nor child blocks."
          ]

    _ ->
      []

expectNoSem :: [Int] -> Block spec -> Text -> [ValidationIssue]
expectNoSem path blk errMsg =
  case semBk blk of
    Nothing -> []
    Just otherSem ->
      [ mkError path (Just (kindBk blk))
          (errMsg <> " Found " <> renderSemantics otherSem <> ".")
      ]

expectSem ::
  [Int] ->
  Block spec ->
  (SemanticsBlk -> Bool) ->
  Text ->
  [ValidationIssue]
expectSem path blk predSem errMsg =
  case semBk blk of
    Just sem
      | predSem sem -> []
      | otherwise ->
          [ mkError path (Just (kindBk blk))
              (errMsg <> " Found " <> renderSemantics sem <> ".")
          ]
    Nothing ->
      [ mkError path (Just (kindBk blk)) errMsg ]

validateChildKinds ::
  [Int] ->
  Block spec ->
  (KindBlk -> Bool) ->
  Text ->
  [ValidationIssue]
validateChildKinds path blk predKind errMsg =
  [ mkError (path <> [idx]) (Just (kindBk child)) errMsg
  | (idx, child) <- zip [0 :: Int ..] (childrenBk blk)
  , not (predKind (kindBk child))
  ]

requireNoChildren :: [Int] -> Block spec -> Text -> [ValidationIssue]
requireNoChildren path blk errMsg =
  if null (childrenBk blk)
    then []
    else [mkError path (Just (kindBk blk)) errMsg]

requireNoContentWarn :: [Int] -> Block spec -> Text -> [ValidationIssue]
requireNoContentWarn path blk warnMsg =
  case contentBk blk of
    Nothing -> []
    Just _ -> [mkWarning path (Just (kindBk blk)) warnMsg]

hasVisiblePayload :: Block spec -> Bool
hasVisiblePayload blk =
  case contentBk blk of
    Just _ -> True
    Nothing -> not (null (childrenBk blk))

isHeadingSem :: SemanticsBlk -> Bool
isHeadingSem sem =
  case sem of
    HeadingSB _ -> True
    _ -> False

isListSem :: SemanticsBlk -> Bool
isListSem sem =
  case sem of
    ListSB _ -> True
    _ -> False

isListItemSem :: SemanticsBlk -> Bool
isListItemSem sem =
  case sem of
    ListItemSB _ -> True
    _ -> False

isCodeSem :: SemanticsBlk -> Bool
isCodeSem sem =
  case sem of
    CodeSB _ -> True
    _ -> False

isTableSem :: SemanticsBlk -> Bool
isTableSem sem =
  case sem of
    TableSB _ -> True
    _ -> False

isTableCellSem :: SemanticsBlk -> Bool
isTableCellSem sem =
  case sem of
    TableCellSB _ -> True
    _ -> False

isFigureSem :: SemanticsBlk -> Bool
isFigureSem sem =
  case sem of
    FigureSB _ -> True
    _ -> False

isImageSem :: SemanticsBlk -> Bool
isImageSem sem =
  case sem of
    ImageSB _ -> True
    _ -> False

isConversationSem :: SemanticsBlk -> Bool
isConversationSem sem =
  case sem of
    ConversationSB _ -> True
    _ -> False

isMessageSem :: SemanticsBlk -> Bool
isMessageSem sem =
  case sem of
    MessageSB _ -> True
    _ -> False

isListItemKind :: KindBlk -> Bool
isListItemKind kind =
  kind == ListItemKB

isTableRowKind :: KindBlk -> Bool
isTableRowKind kind =
  kind == TableRowKB

isTableCellKind :: KindBlk -> Bool
isTableCellKind kind =
  kind == TableCellKB

isMessageKind :: KindBlk -> Bool
isMessageKind kind =
  kind == MessageKB

mkError :: [Int] -> Maybe KindBlk -> Text -> ValidationIssue
mkError path kind msg =
  ValidationIssue
    { levelVi = ErrorVL
    , pathVi = path
    , kindVi = kind
    , msgVi = msg
    }

mkWarning :: [Int] -> Maybe KindBlk -> Text -> ValidationIssue
mkWarning path kind msg =
  ValidationIssue
    { levelVi = WarningVL
    , pathVi = path
    , kindVi = kind
    , msgVi = msg
    }

renderPath :: [Int] -> Text
renderPath path =
  case path of
    [] -> "root"
    _ ->
      "root"
      <> T.concat [ "." <> T.pack (show idx) | idx <- path ]

renderKind :: KindBlk -> Text
renderKind kind =
  case kind of
    ContainerKB -> "ContainerKB"
    HeadingKB -> "HeadingKB"
    ParagraphKB -> "ParagraphKB"
    ListKB -> "ListKB"
    ListItemKB -> "ListItemKB"
    QuoteKB -> "QuoteKB"
    CodeKB -> "CodeKB"
    TableKB -> "TableKB"
    TableRowKB -> "TableRowKB"
    TableCellKB -> "TableCellKB"
    FigureKB -> "FigureKB"
    ImageKB -> "ImageKB"
    RuleKB -> "RuleKB"
    NoteKB FootnoteNK -> "NoteKB FootnoteNK"
    NoteKB EndnoteNK -> "NoteKB EndnoteNK"
    ConversationKB -> "ConversationKB"
    MessageKB -> "MessageKB"
    CustomKB tag -> "CustomKB " <> tag

renderSemantics :: SemanticsBlk -> Text
renderSemantics sem =
  case sem of
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