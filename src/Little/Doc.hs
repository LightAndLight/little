module Little.Doc where

import Little (Document(..), Node (..), DefineFragmentNode (..))
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder (Builder)
import Data.Text (Text)

renderDocument :: Document -> Lazy.Text
renderDocument (Document nodes) =
  Builder.toLazyText $ foldMap renderNode nodes

renderNode :: Node -> Builder
renderNode node =
  case node of
    Text t ->
      Builder.fromText t
    Nodes nodes ->
      foldMap renderNode nodes
    DefineFragment path mName content ->
      foldMap (renderDefineFragmentNode path mName) content
    AppendFragment path mName content ->
      foldMap (renderDefineFragmentNode path mName) content
    FragmentRef path name ->
      Builder.fromString path <> ":" <> Builder.fromText name

renderDefineFragmentNode :: FilePath -> Maybe Text -> DefineFragmentNode -> Builder
renderDefineFragmentNode path mName node =
  case node of
    DefineFragmentNodeText t ->
      Builder.fromText t
    DefineFragmentNodeNodes nodes ->
      foldMap (renderDefineFragmentNode path mName) nodes
    DefineFragmentNodeFragmentId ->
      Builder.fromString path <> foldMap (\name -> ":" <> Builder.fromText name) mName
    DefineFragmentNodeFragmentRef refPath refName ->
      Builder.fromString refPath <> ":" <> Builder.fromText refName
    DefineFragmentNodeCode nodes ->
      foldMap (renderDefineFragmentNode path mName) nodes
    DefineFragmentNodeUncode nodes ->
      foldMap (renderDefineFragmentNode path mName) nodes
