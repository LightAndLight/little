{-# language OverloadedStrings #-}
module Example.Expr (expr) where

import Little

expr :: Document
expr =
  document
  [ "# Expression example"
  , ""
  , "There are two ways to view this document:"
  , ""
  , "1. A Markdown document explaining some Haskell code"
  , "2. The Haskell code that was explained in the Markdown document"
  , ""
  , "First, I introduce a fragment for the file `Main.hs`:"
  , ""
  , "```haskell"
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
  , "```"
  , ""
  , "Next I define an `Expr` datatype:"
  , ""
  , "```haskell"
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
  , "```"
  , ""
  , nodes
      [ "The following function pretty-prints `Expr`s. Its code is appended to the `"
      , frag "Main.hs" "definitions"
      , "` fragment.\n"
      ]
  , ""
  , "```haskell"
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
  , "```"
  , ""
  , "Finally, the entrypoint for the Main module:"
  , ""
  , "```haskell"
  , define
      "Main.hs"
      (Just "entrypoint")
      [ nodes ["-- <<", fragId, ">>=\n"]
      , code
          [ "main :: IO ()\n"
          , "main = putStrLn . prettyExpr $ Lam \"x\" (Var \"x\")\n"
          ]
      ]
  , "```"
  ]
