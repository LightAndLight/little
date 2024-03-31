module Main where

import qualified Example.Embed
import qualified Example.Expr
import qualified Example.Script
import qualified Little.Doc
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified Data.Map as Map
import qualified Little.Code
import Data.Foldable (for_)

main :: IO ()
main = do
  putStrLn "--- Example.Expr ---"
  exampleExpr
  putStrLn "--- Example.Script ---"
  exampleScript
  putStrLn "--- Example.Embed ---"
  exampleEmbed

exampleExpr :: IO ()
exampleExpr = do
  result <- Little.Doc.renderDocument Example.Expr.document
  case result of
    Left err ->
      print err
    Right doc ->
      Text.Lazy.IO.putStrLn doc

  case Little.Code.renderDocument Example.Expr.document of
    Left err ->
      print err
    Right files ->
      for_ (Map.toList files) $ \(path, content) -> do
        putStrLn $ path <> ":"
        Text.Lazy.IO.putStrLn content

exampleScript :: IO ()
exampleScript = do
  result <- Little.Doc.renderDocument Example.Script.document
  case result of
    Left err ->
      print err
    Right doc ->
      Text.Lazy.IO.putStrLn doc

  case Little.Code.renderDocument Example.Script.document of
    Left err ->
      print err
    Right files ->
      for_ (Map.toList files) $ \(path, content) -> do
        putStrLn $ path <> ":"
        Text.Lazy.IO.putStrLn content

exampleEmbed :: IO ()
exampleEmbed = do
  result <- Little.Doc.renderDocument Example.Embed.document
  case result of
    Left err ->
      print err
    Right doc ->
      Text.Lazy.IO.putStrLn doc

  case Little.Code.renderDocument Example.Embed.document of
    Left err ->
      print err
    Right files ->
      for_ (Map.toList files) $ \(path, content) -> do
        putStrLn $ path <> ":"
        Text.Lazy.IO.putStrLn content
