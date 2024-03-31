{-# language OverloadedStrings #-}
module Example.Expr (document) where

import Little

document :: Document
document =
  Document
  [ "# Expression example\n"
  , "\n"
  , "There are two ways to view this document:\n"
  , "\n"
  , "1. A Markdown document explaining some Haskell code\n"
  , "2. The Haskell code that was explained in the Markdown document\n"
  , "\n"
  , "First, I introduce a fragment for the file `Main.hs`:\n"
  , "\n"
  , "```haskell\n"
  , Fragment
      Define
      "Main.hs"
      Nothing
      [ FragmentNodeNodes ["-- <<", FragmentNodeFragmentId, ">>=\n"]
      , FragmentNodeCode
          [ "module Main where\n"
          , "\n"
          , FragmentNodeNodes
              [ FragmentNodeUncode ["-- <<"]
              , FragmentNodeFragmentRef "Main.hs" "definitions"
              , FragmentNodeUncode [">>\n"]
              ]
          , "\n"
          , FragmentNodeNodes
              [ FragmentNodeUncode ["-- <<"]
              , FragmentNodeFragmentRef "Main.hs" "entrypoint"
              , FragmentNodeUncode [">>\n"]
              ]
          ]
      ]
  , "```\n"
  , "\n"
  , "Next I define an `Expr` datatype:\n"
  , "\n"
  , "```haskell\n"
  , Fragment
      Define
      "Main.hs"
      (Just "definitions")
      [ FragmentNodeNodes ["-- <<", FragmentNodeFragmentId, ">>=\n"]
      , FragmentNodeCode
          [ "data Expr\n"
          , "  = Var String\n"
          , "  | Lam String Expr\n"
          , "  | App Expr Expr\n"
          ]
      ]
  , "```\n"
  , "\n"
  , Nodes
      [ "The following function pretty-prints `Expr`s. Its code is appended to the `"
      , FragmentRef "Main.hs" "definitions"
      , "` fragment.\n"
      ]
  , "\n"
  , "```haskell\n"
  , Fragment
      Append
      "Main.hs"
      (Just "definitions")
      [ FragmentNodeNodes ["-- <<", FragmentNodeFragmentId, ">>+=\n"]
      , FragmentNodeCode
          [ "\n"
          , "prettyExpr :: Expr -> String\n"
          , "prettyExpr expr =\n"
          , "  case expr of\n"
          , "    Var name -> name\n"
          , "    Lam name body -> \"\\\\\" <> name <> \" -> \" <> prettyExpr body\n"
          , "    App f x -> prettyExpr f <> prettyExpr x\n"
          ]
      ]
  , "```\n"
  , "\n"
  , "Finally, the entrypoint for the Main module:\n"
  , "\n"
  , "```haskell\n"
  , Fragment
      Define
      "Main.hs"
      (Just "entrypoint")
      [ FragmentNodeNodes ["-- <<", FragmentNodeFragmentId, ">>=\n"]
      , FragmentNodeCode
          [ "main :: IO ()\n"
          , "main = putStrLn . prettyExpr $ Lam \"x\" (Var \"x\")\n"
          ]
      ]
  , "```\n"
  ]
