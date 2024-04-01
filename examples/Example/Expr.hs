{-# language OverloadedStrings #-}
module Example.Expr (expr) where

import Little

expr :: Document
expr =
  document
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
  , define 
      "Main.hs"
      Nothing
      [ nodes ["-- <<", fragId, ">>=\n"]
      , code
          [ "module Main where\n"
          , "\n"
          , nodes [uncode ["-- <<"], frag "Main.hs" "definitions", uncode [">>\n"]]
          , "\n"
          , nodes [uncode ["-- <<"], frag "Main.hs" "entrypoint", uncode [">>\n"]]
          ]
      ]
  , "```\n"
  , "\n"
  , "Next I define an `Expr` datatype:\n"
  , "\n"
  , "```haskell\n"
  , define
      "Main.hs"
      (Just "definitions")
      [ nodes ["-- <<", fragId, ">>=\n"]
      , code
          [ "data Expr\n"
          , "  = Var String\n"
          , "  | Lam String Expr\n"
          , "  | App Expr Expr\n"
          ]
      ]
  , "```\n"
  , "\n"
  , nodes
      [ "The following function pretty-prints `Expr`s. Its code is appended to the `"
      , frag "Main.hs" "definitions"
      , "` fragment.\n"
      ]
  , "\n"
  , "```haskell\n"
  , append
      "Main.hs"
      (Just "definitions")
      [ nodes ["-- <<", fragId, ">>+=\n"]
      , code
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
  , define
      "Main.hs"
      (Just "entrypoint")
      [ nodes ["-- <<", fragId, ">>=\n"]
      , code
          [ "main :: IO ()\n"
          , "main = putStrLn . prettyExpr $ Lam \"x\" (Var \"x\")\n"
          ]
      ]
  , "```\n"
  ]
