module HBDoc.Serialize.Types where

import Data.Text (Text)
import Data.Int (Int32)
import HBDoc.Core.Types (HBDoc)

data SerializeInfo docSpec blkSpec = SerializeInfo {
      userName :: Text
    , mbDocID :: Maybe Int32
    , contentType :: Text
    , size :: Int32
    , key :: Text
    , shaHex :: Text
    , originalName :: Text
    , document :: HBDoc docSpec blkSpec
  }
