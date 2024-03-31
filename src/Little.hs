{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Little where

import Data.Text (Text)
import Data.String (IsString(..))

newtype Document = Document [Node]
  deriving (Eq, Show)

data Node
  = Text Text
  | Nodes [Node]
  | DefineFragment FilePath (Maybe Text) [DefineFragmentNode]
  | AppendFragment FilePath (Maybe Text) [DefineFragmentNode]
  | FragmentRef FilePath Text
  deriving (Eq, Show)

instance IsString Node where
  fromString = Text . fromString

data DefineFragmentNode
  = DefineFragmentNodeText Text
  | DefineFragmentNodeNodes [DefineFragmentNode]
  | DefineFragmentNodeFragmentId
  | DefineFragmentNodeFragmentRef FilePath Text
  | DefineFragmentNodeCode [DefineFragmentNode]
  | DefineFragmentNodeUncode [DefineFragmentNode]
  deriving (Eq, Show)

instance IsString DefineFragmentNode where
  fromString = DefineFragmentNodeText . fromString
