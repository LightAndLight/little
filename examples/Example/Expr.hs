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
  , DefineFragment 
      "Main.hs"
      Nothing
      [ DefineFragmentNodeNodes ["-- <<", DefineFragmentNodeFragmentId, ">>=\n"]
      , DefineFragmentNodeCode
          [ "module Main where\n"
          , "\n"
          , DefineFragmentNodeNodes
              [ DefineFragmentNodeUncode ["-- <<"]
              , DefineFragmentNodeFragmentRef "Main.hs" "definitions"
              , DefineFragmentNodeUncode [">>\n"]
              ]
          , "\n"
          , DefineFragmentNodeNodes
              [ DefineFragmentNodeUncode ["-- <<"]
              , DefineFragmentNodeFragmentRef "Main.hs" "entrypoint"
              , DefineFragmentNodeUncode [">>\n"]
              ]
          ]
      ]
  , "```\n"
  , "\n"
  , "Next I define an `Expr` datatype:\n"
  , "\n"
  , "```haskell\n"
  , DefineFragment 
      "Main.hs"
      (Just "definitions")
      [ DefineFragmentNodeNodes ["-- <<", DefineFragmentNodeFragmentId, ">>=\n"]
      , DefineFragmentNodeCode
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
  , AppendFragment 
      "Main.hs"
      (Just "definitions")
      [ DefineFragmentNodeNodes ["-- <<", DefineFragmentNodeFragmentId, ">>+=\n"]
      , DefineFragmentNodeCode
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
  , DefineFragment 
      "Main.hs"
      (Just "entrypoint")
      [ DefineFragmentNodeNodes ["-- <<", DefineFragmentNodeFragmentId, ">>=\n"]
      , DefineFragmentNodeCode
          [ "main :: IO ()\n"
          , "main = putStrLn . prettyExpr $ Lam \"x\" (Var \"x\")\n"
          ]
      ]
  , "```\n"
  ]
