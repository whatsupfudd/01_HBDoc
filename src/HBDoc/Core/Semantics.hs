{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HBDoc.Core.Semantics where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import HBDoc.Core.BlockKind
import HBDoc.Core.ListSemantics

data CodeSemantics = CodeSemantics {
    languageCds :: !(Maybe Text)
  , infoStringCds :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data TableSemantics = TableSemantics {
    headerRowsTbs :: !(Maybe Int32)
  , captionTbs :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data TableCellSemantics = TableCellSemantics {
    rowSpanTcs :: !(Maybe Int32)
  , colSpanTcs :: !(Maybe Int32)
  , alignmentTcs :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype FigureSemantics = FigureSemantics {
    captionFgs :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype ImageSemantics = ImageSemantics {
    altTextIms :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data NoteSemantics = NoteSemantics {
    kindNts :: !NoteKind
  , noteIdNts :: !(Maybe Int32)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

newtype ConversationSemantics = ConversationSemantics {
    threadIdCvs :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data MessageSemantics = MessageSemantics {
    roleMsg :: !(Maybe Text)
  , authorMsg :: !(Maybe Text)
  , tsMsg :: !(Maybe Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data CustomSemantics = CustomSemantics {
    tagCst :: !Text
  , fieldsCst :: !(Map Text Text)
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data SemanticsBlk =
    HeadingSB HeadingSemantics
  | ListSB ListSemantics
  | ListItemSB ListItemSemantics
  | CodeSB CodeSemantics
  | TableSB TableSemantics
  | TableCellSB TableCellSemantics
  | FigureSB FigureSemantics
  | ImageSB ImageSemantics
  | NoteSB NoteSemantics
  | ConversationSB ConversationSemantics
  | MessageSB MessageSemantics
  | CustomSB CustomSemantics
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)