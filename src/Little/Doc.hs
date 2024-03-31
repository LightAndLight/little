module Little.Doc where

import Little (Document(..), Node (..), FragmentNode (..), FragmentAction (..), RunNode (..))
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder (Builder)
import Data.Text (Text)
import Data.List (isSuffixOf)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (Ap(..))
import System.Process (readProcess)
import qualified Data.Text.Lazy as Text.Lazy
import Data.List.NonEmpty (NonEmpty, nonEmpty)

renderDocument :: MonadIO m => Document -> m (Either (NonEmpty RenderError) Lazy.Text)
renderDocument (Document nodes) = do
  (errs, content) <- getAp (foldMap (Ap . renderNode) nodes)
  case nonEmpty errs of
    Nothing ->
      pure . Right $ Builder.toLazyText content
    Just errs' ->
      pure $ Left errs'

data RenderError
  = CommandOutputMismatch
      -- | Command
      String
      -- | Expected output
      String
      -- | Actual output
      String
  deriving Show

renderNode :: MonadIO m => Node -> m ([RenderError], Builder)
renderNode node =
  case node of
    Text t ->
      pure (mempty, Builder.fromText t)
    Nodes nodes ->
      getAp $ foldMap (Ap . renderNode) nodes
    Fragment Define path mName content ->
      pure (mempty, foldMap (renderFragmentNode path mName) content)
    Fragment Append path mName content ->
      pure (mempty, foldMap (renderFragmentNode path mName) content)
    FragmentRef path name ->
      pure (mempty, Builder.fromString path <> ":" <> Builder.fromText name)
    Run cmd args content -> do
      output <- liftIO $ readProcess cmd args ""
      let command = cmd <> foldMap ((" " <>) . renderCommandArg) args
      pure $ foldMap (renderRunNode command output) content
      where
        renderCommandArg :: String -> String
        renderCommandArg arg
          | '"' `elem` arg =
              "\"" <>
              concatMap (\c -> if c == '"' then "\\\"" else [c]) arg <> 
              "\""
          | ' ' `elem` arg = "\"" <> arg <> "\""
          | otherwise = arg

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

renderRunNode :: String -> String -> RunNode -> ([RenderError], Builder)
renderRunNode command output node =
  case node of
    RunNodeText t ->
      pure $ Builder.fromText t
    RunNodeNodes nodes ->
      foldMap (renderRunNode command output) nodes
    RunNodeCommand ->
      pure $ Builder.fromString command
    RunNodeOutput ->
      pure $ Builder.fromString output
    RunNodeExpected nodes -> do
      actual <-
        Text.Lazy.unpack . Builder.toLazyText <$>
        foldMap (renderRunNode command output) nodes
      if actual == output
        then (mempty, Builder.fromString actual)
        else ([CommandOutputMismatch command output actual], Builder.fromString actual)
