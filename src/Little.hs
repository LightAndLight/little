module Little 
  ( -- * Documents
  Document
  , document
  
  -- * Nodes
  , Node
  
  -- ** Text nodes
  , HasText(text)
  
  -- ** Nested nodes
  , HasNodes(nodes)
  
  -- ** Fragment manipulation
  , FragmentNode
  , define
  , append
  , fragId
  , code
  , uncode

  -- ** Fragment references
  , HasFrag(frag)

  -- ** Command execution
  , RunNode
  , run
  , command
  , output
  , expected
  )
where

import Little.Types
import Data.String (IsString, fromString)
import Data.Text (Text)

document :: [Node] -> Document
document = Document

class IsString a => HasText a where
  text :: String -> a

class HasText a => HasNodes a where
  nodes :: [a] -> a

instance HasText Node where
  text = Text . fromString

instance HasNodes Node where
  nodes = Nodes

instance HasText FragmentNode where
  text = FragmentNodeText . fromString

instance HasNodes FragmentNode where
  nodes = FragmentNodeNodes

define :: FilePath -> Maybe Text -> [FragmentNode] -> Node
define = Fragment Define

append :: FilePath -> Maybe Text -> [FragmentNode] -> Node
append = Fragment Append

fragId :: FragmentNode
fragId = FragmentNodeFragmentId

code :: [FragmentNode] -> FragmentNode
code = FragmentNodeCode

uncode :: [FragmentNode] -> FragmentNode
uncode = FragmentNodeUncode

class HasNodes a => HasFrag a where
  frag :: FilePath -> Text -> a

instance HasFrag Node where
  frag = FragmentRef

instance HasFrag FragmentNode where
  frag = FragmentNodeFragmentRef

instance HasText RunNode where
  text = RunNodeText . fromString

instance HasNodes RunNode where
  nodes = RunNodeNodes

run :: String -> [String] -> [RunNode] -> Node
run = Run

command :: RunNode
command = RunNodeCommand

output :: RunNode
output = RunNodeOutput

expected :: [RunNode] -> RunNode
expected = RunNodeExpected
