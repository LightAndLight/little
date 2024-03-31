module Little.Doc where

import Little (Document(..), Node (..), FragmentNode (..), FragmentAction (..))
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder (Builder)
import Data.Text (Text)
import Data.List (isSuffixOf)

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
    Fragment Define path mName content ->
      foldMap (renderFragmentNode path mName) content
    Fragment Append path mName content ->
      foldMap (renderFragmentNode path mName) content
    FragmentRef path name ->
      Builder.fromString path <> ":" <> Builder.fromText name

renderFragmentNode :: FilePath -> Maybe Text -> FragmentNode -> Builder
renderFragmentNode path mName node =
  case node of
    FragmentNodeText t ->
      Builder.fromText t
    FragmentNodeNodes nodes ->
      foldMap (renderFragmentNode path mName) nodes
    FragmentNodeFragmentId ->
      Builder.fromString path <> foldMap (\name -> ":" <> Builder.fromText name) mName
    FragmentNodeFragmentRef refPath refName ->
      if refPath `isSuffixOf` path
      then Builder.fromText refName
      else Builder.fromString refPath <> ":" <> Builder.fromText refName
    FragmentNodeCode nodes ->
      foldMap (renderFragmentNode path mName) nodes
    FragmentNodeUncode nodes ->
      foldMap (renderFragmentNode path mName) nodes
