module Main where

import qualified Example.Expr
import qualified Little.Doc
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified Data.Map as Map
import qualified Little.Code
import Data.Foldable (for_)

main :: IO ()
main = do
  Text.Lazy.IO.putStrLn $ Little.Doc.renderDocument Example.Expr.document

  case Little.Code.renderDocument Example.Expr.document of
    Left err ->
      print err
    Right files ->
      for_ (Map.toList files) $ \(path, content) -> do
        putStrLn $ path <> ":"
        Text.Lazy.IO.putStrLn content
