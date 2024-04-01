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
      [ line ["-- <<", fragId, ">>="]
      , code
          [ "module Main where"
          , ""
          , nodes [uncode ["-- <<"], frag "Main.hs" "definitions", uncode [">>\n"]]
          , ""
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
      [ line ["-- <<", fragId, ">>="]
      , code
          [ "data Expr"
          , "  = Var String"
          , "  | Lam String Expr"
          , "  | App Expr Expr"
          ]
      ]
  , "```"
  , ""
  , para
      [ "The following function pretty-prints `Expr`s."
      , nodes ["Its code is appended to the `", frag "Main.hs" "definitions", "` fragment."]
      ]
  , ""
  , "```haskell"
  , append
      "Main.hs"
      (Just "definitions")
      [ line ["-- <<", fragId, ">>+="]
      , code
          [ ""
          , "prettyExpr :: Expr -> String"
          , "prettyExpr expr ="
          , "  case expr of"
          , "    Var name -> name"
          , "    Lam name body -> \"\\\\\" <> name <> \" -> \" <> prettyExpr body"
          , "    App f x -> prettyExpr f <> prettyExpr x"
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
      [ line ["-- <<", fragId, ">>="]
      , code
          [ "main :: IO ()"
          , "main = putStrLn . prettyExpr $ Lam \"x\" (Var \"x\")"
          ]
      ]
  , "```"
  ]
