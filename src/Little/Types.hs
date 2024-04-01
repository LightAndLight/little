module Little.Types where

import Data.Text (Text)
import Data.String (IsString(..))

newtype Document = Document [Node]
  deriving (Eq, Show)

data Node
  = Text Text
  | Nodes [Node]
  | Fragment FragmentAction FilePath (Maybe Text) [FragmentNode]
  | FragmentRef FilePath Text
  | Run String [String] [RunNode]
  deriving (Eq, Show)

instance IsString Node where
  fromString = Text . fromString

data FragmentAction = Define | Append
  deriving (Eq, Show)

data FragmentNode
  = FragmentNodeText Text
  | FragmentNodeNodes [FragmentNode]
  | FragmentNodeFragmentId
  | FragmentNodeFragmentRef FilePath Text
  | FragmentNodeCode [FragmentNode]
  | FragmentNodeUncode [FragmentNode]
  deriving (Eq, Show)

instance IsString FragmentNode where
  fromString = FragmentNodeText . fromString

data RunNode
  = RunNodeText Text
  | RunNodeNodes [RunNode]
  | RunNodeCommand
  | RunNodeOutput
  | RunNodeExpected [RunNode]
  deriving (Eq, Show)

instance IsString RunNode where
  fromString = RunNodeText . fromString