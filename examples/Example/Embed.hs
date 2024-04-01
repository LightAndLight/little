{-# language OverloadedStrings #-}
module Example.Embed (embed) where

import Little

embed :: Document
embed =
  document
  [ "# Embedding program results"
  , ""
  , "Little documents can call other programs and embed their results."
  , ""
  , "* ```"
  , run
      "mktemp"
      ["--tmpdir"]
      [ line ["  $ ", command]
      , nodes ["  ", output]
      ]
  , "  ```"
  , ""
  , "* ```"
  , run
      "mktemp"
      ["--tmpdir"]
      [ line ["  $ ", command]
      , nodes ["  ", output]
      ]
  , "  ```"
  , ""
  , "* ```"
  , run
      "mktemp"
      ["--tmpdir"]
      [ line ["  $ ", command]
      , nodes ["  ", output]
      ]
  , "  ```"
  , ""
  , para
      ["If it's important that the command gives a *specific* output, you can write that output down and *check* that it matches the output of the command."
      , "The following code is written in a way that causes Little to raise an error when the command output differs from what is documented:"
      ]
  , ""
  , "```"
  , run
      "echo"
      ["Hello, world!"]
      [ line ["$ ", command]
      , expected ["Hello, world!\n"]
      ]
  , "```"
  ]
