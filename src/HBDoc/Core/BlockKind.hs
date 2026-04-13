{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Core.BlockKind where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data NoteKind =
    FootnoteNK
  | EndnoteNK
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Flat block tag only.
-- No heading level or list-item level is stored here.
-- Those move into typed semantics.
data KindBlk =
    ContainerKB
  | HeadingKB
  | ParagraphKB
  | ListKB
  | ListItemKB
  | QuoteKB
  | CodeKB
  | TableKB
  | TableRowKB
  | TableCellKB
  | FigureKB
  | ImageKB
  | RuleKB
  | NoteKB NoteKind
  | ConversationKB
  | MessageKB
  | CustomKB Text
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)