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
      [ nodes ["  $ ", command, "\n"]
      , nodes ["  ", output]
      ]
  , "  ```"
  , ""
  , "* ```"
  , run
      "mktemp"
      ["--tmpdir"]
      [ nodes ["  $ ", command, "\n"]
      , nodes ["  ", output]
      ]
  , "  ```"
  , ""
  , "* ```"
  , run
      "mktemp"
      ["--tmpdir"]
      [ nodes ["  $ ", command, "\n"]
      , nodes ["  ", output]
      ]
  , "  ```"
  , ""
  , nodes
      ["If it's important that the command gives a *specific* output, you can write that output down and *check* that it matches the output of the command."
      , " "
      , "The following code is written in a way that causes Little to raise an error when the command output differs from what is documented:"
      , "\n"
      ]
  , ""
  , "```"
  , run
      "echo"
      ["Hello, world!"]
      [ nodes ["$ ", command, "\n"]
      , expected ["Hello, world!\n"]
      ]
  , "```"
  ]
