module Little.Code where

import Little (Document(..), Node (..), FragmentNode (..))
import qualified Little (FragmentAction(..))
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder (Builder)
import Data.Maybe (mapMaybe, isNothing)
import Data.Map (Map)
import Data.Text (Text)
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Control.Monad (guard)

renderDocument :: Document -> Either (NonEmpty RenderError) (Map FilePath Lazy.Text)
renderDocument (Document nodes) =
  let (errs, files) = go (getMonoidalMap $ foldMap renderNode nodes) in
  case nonEmpty errs of
    Nothing -> Right files
    Just errs' -> Left errs'
  where
    go :: Map (FilePath, Maybe Text) Fragment -> ([RenderError], Map FilePath Lazy.Text)
    go fragments =
      Map.fromList <$>
      traverse
        (\(path, content) -> (,) path <$> renderFile fragments path content)
        (mapMaybe
          (\((path, mName), content) -> do
            guard $ isNothing mName
            pure (path, content)
          )
          (Map.toList fragments)
        )

    renderFile ::
      Map (FilePath, Maybe Text) Fragment ->
      FilePath ->
      Fragment ->
      ([RenderError], Lazy.Text)
    renderFile fragments path content =
      case content of
        Define content' ->
          Builder.toLazyText <$>
          foldMap (renderFragmentContent fragments) content'
        Append{} ->
          ([UndefinedFile path], mempty)
        Error err ->
          ([FragmentError err], mempty)

    renderFragment ::
      Map (FilePath, Maybe Text) Fragment ->
      FilePath ->
      Text ->
      Fragment ->
      ([RenderError], Builder)
    renderFragment fragments path name content =
      case content of
        Define content' ->
          foldMap (renderFragmentContent fragments) content'
        Append{} ->
          ([UndefinedFragment path name], mempty)
        Error err ->
          ([FragmentError err], mempty)

    renderFragmentContent ::
      Map (FilePath, Maybe Text) Fragment ->
      FragmentContent ->
      ([RenderError], Builder)
    renderFragmentContent fragments content =
      case content of
        FragmentContentText t ->
          (mempty, t)
        FragmentContentRef path name ->
          case Map.lookup (path, Just name) fragments of
            Nothing ->
              ([UndefinedFragment path name], mempty)
            Just content' ->
              renderFragment fragments path name content'

data RenderError
  = UndefinedFile FilePath
  | UndefinedFragment FilePath Text
  | FragmentError FragmentError
  deriving Show

data Fragment
  = Define [FragmentContent]
  | Append [FragmentContent]
  | Error FragmentError

data FragmentContent
  = FragmentContentText Builder
  | FragmentContentRef FilePath Text

data FragmentError
  = Redefined
  | Undefined
  deriving Show

instance Semigroup Fragment where
  Define{} <> Define{} = Error Redefined
  Define a <> Append b = Define (a <> b)
  Define{} <> Error e = Error e

  Append{} <> Define{} = Error Undefined
  Append a <> Append b = Append (a <> b)
  Append{} <> Error e = Error e

  Error e <> _ = Error e

renderNode :: Node -> MonoidalMap (FilePath, Maybe Text) Fragment
renderNode node =
  case node of
    Text{} ->
      mempty
    Nodes nodes ->
      foldMap renderNode nodes
    Fragment Little.Define path mName nodes ->
      MonoidalMap . Map.singleton (path, mName) . Define $
        foldMap (renderFragmentNode False) nodes
    Fragment Little.Append path mName nodes ->
      MonoidalMap . Map.singleton (path, mName) . Append $
        foldMap (renderFragmentNode False) nodes
    FragmentRef{} ->
      mempty
    Run{} ->
      mempty

renderFragmentNode :: Bool -> FragmentNode -> [FragmentContent]
renderFragmentNode inCode node =
  case node of
    FragmentNodeText t ->
      if inCode
      then [FragmentContentText $ Builder.fromText t]
      else mempty
    FragmentNodeNodes nodes ->
      foldMap (renderFragmentNode inCode) nodes
    FragmentNodeFragmentId ->
      mempty
    FragmentNodeFragmentRef path name ->
      if inCode
      then [FragmentContentRef path name]
      else mempty
    FragmentNodeCode nodes ->
      foldMap (renderFragmentNode True) nodes
    FragmentNodeUncode{} ->
      mempty
