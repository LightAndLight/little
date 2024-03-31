{-# language OverloadedStrings #-}
module Example.Script (document) where

import Little

document :: Document
document =
  Document
  [ "# A literate Bash script\n"
  , "\n"
  , "This script describes how to clean up `docker` resources.\n"
  , "\n"
  , "1. Remove containers\n"
  , "\n"
  , "   ```bash\n"
  , Fragment
      Define
      "script.sh"
      Nothing
      [ FragmentNodeNodes ["   # <<", FragmentNodeFragmentId, ">>=\n"]
      , FragmentNodeNodes ["   ", FragmentNodeCode ["docker ps --all --format \"{{.ID}}\" | xargs docker rm\n"]]
      ]
  , "   ```\n"
  , "\n"
  , "2. Remove images\n"
  , "\n"
  , "   ```bash\n"
  , Fragment
      Append
      "script.sh"
      Nothing
      [ FragmentNodeNodes ["   # <<", FragmentNodeFragmentId, ">>+=\n"]
      , FragmentNodeNodes ["   ", FragmentNodeCode ["docker images --format \"{{.ID}}\" | xargs docker rmi -f\n"]]
      ]
  , "   ```\n"
  , "\n"
  , "3. Remove volumes\n"
  , "\n"
  , "   ```bash\n"
  , Fragment
      Append
      "script.sh"
      Nothing
      [ FragmentNodeNodes ["   # <<", FragmentNodeFragmentId, ">>+=\n"]
      , FragmentNodeNodes ["   ", FragmentNodeCode ["docker volume prune\n"]]
      ]
  , "   ```\n"
  , "\n"
  , "4. Remove build cache\n"
  , "\n"
  , "   ```bash\n"
  , Fragment
      Append
      "script.sh"
      Nothing
      [ FragmentNodeNodes ["   # <<", FragmentNodeFragmentId, ">>+=\n"]
      , FragmentNodeNodes ["   ", FragmentNodeCode ["docker build prune\n"]]
      ]
  , "   ```\n"
  ]
