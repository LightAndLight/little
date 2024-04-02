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
  , line
  , para
  
  -- ** Fragment manipulation
  , define
  , append
  , FragmentNode
  , fragId
  , code
  , uncode

  -- ** Fragment references
  , HasFrag(frag)

  -- ** Command execution
  , run
  , RunNode
  , command
  , output
  , expected
  )
where

import Little.Types
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.List (intersperse)

-- | Note: 'text' child nodes have a newline appended.
document :: [Node] -> Document
document =
  Document .
  fmap
    (\node -> case node of
      Text{} -> Nodes [node, "\n"]
      _ -> node
    )

class IsString a => HasText a where
  text :: String -> a

class HasText a => HasNodes a where
  nodes :: [a] -> a

-- | Some 'nodes', with a newline after the last node.
line :: HasNodes a => [a] -> a
line as = nodes [nodes as, text "\n"]

-- | Space-separated 'nodes', followed by a newline.
para :: HasNodes a => [a] -> a
para as = nodes [nodes $ intersperse " " as, "\n"]

instance HasText Node where
  text = Text . fromString

instance HasNodes Node where
  nodes = Nodes

instance HasText FragmentNode where
  text = FragmentNodeText . fromString

instance HasNodes FragmentNode where
  nodes = FragmentNodeNodes

-- | Define a fragment.
--
-- It is an error to re-'define' a fragment.
define ::
  -- | File the fragment belongs to.
  FilePath ->
  -- | The name of the fragment within the file.
  --
  -- If omitted, the definition is for the entire file.
  Maybe Text ->
  [FragmentNode] ->
  Node
define = Fragment Define

-- | Append to a fragment.
--
-- It is an error to append to an un'define'd fragment.
append ::
  -- | File the fragment belongs to.
  FilePath ->
  -- | The name of the fragment within the file.
  --
  -- If omitted, the definition is for the entire file.
  Maybe Text ->
  [FragmentNode] ->
  Node
append = Fragment Append

-- | The current fragment's ID.
fragId :: FragmentNode
fragId = FragmentNodeFragmentId

-- | Identifies the fragment's code.
--
-- 'FragmentNode's outside 'code' are ignored when rendering the document as source code.
--
-- Note: 'text' children have a newline appended.
code :: [FragmentNode] -> FragmentNode
code =
  FragmentNodeCode .
  fmap
    (\node ->
      case node of
        FragmentNodeText{} -> nodes [node, "\n"]
        _ -> node
    )

-- | Excludes content from the fragment's code.
--
-- Inside 'code', 'uncode' marks content that should be ignored when rendering the document as source code.
-- Outside of 'code', 'uncode' is equivalent to 'nodes'.
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
