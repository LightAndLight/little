{-# language OverloadedStrings #-}
module Example.Embed (document) where

import Little

document :: Document
document =
  Document
  [ "# Embedding program results\n"
  , "\n"
  , "Little documents can call other programs and embed their results.\n"
  , "\n"
  , "* ```\n"
  , Run
      "mktemp"
      ["--tmpdir"]
      [ RunNodeNodes ["  $ ", RunNodeCommand, "\n"]
      , RunNodeNodes ["  ", RunNodeOutput]
      ]
  , "  ```\n"
  , "\n"
  , "* ```\n"
  , Run
      "mktemp"
      ["--tmpdir"]
      [ RunNodeNodes ["  $ ", RunNodeCommand, "\n"]
      , RunNodeNodes ["  ", RunNodeOutput]
      ]
  , "  ```\n"
  , "\n"
  , "* ```\n"
  , Run
      "mktemp"
      ["--tmpdir"]
      [ RunNodeNodes ["  $ ", RunNodeCommand, "\n"]
      , RunNodeNodes ["  ", RunNodeOutput]
      ]
  , "  ```\n"
  , "\n"
  , Nodes
      ["If it's important that the command gives a *specific* output, you can write that output down and *check* that it matches the output of the command."
      , " "
      , "The following code is written in a way that causes Little to raise an error when the command output differs from what is documented:"
      , "\n"
      ]
  , "\n"
  , "```\n"
  , Run
      "echo"
      ["Hello, world!"]
      [ RunNodeNodes ["$ ", RunNodeCommand, "\n"]
      , RunNodeExpected ["Hello, world!\n"]
      ]
  , "```\n"
  ]
