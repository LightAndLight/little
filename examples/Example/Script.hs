{-# language OverloadedStrings #-}
module Example.Script (script) where

import Little

script :: Document
script =
  document
  [ "# A literate Bash script"
  , ""
  , "This script describes how to clean up `docker` resources."
  , ""
  , "1. Remove containers"
  , ""
  , "   ```bash"
  , define
      "script.sh"
      Nothing
      [ nodes ["   # <<", fragId, ">>=\n"]
      , nodes ["   ", code ["docker ps --all --format \"{{.ID}}\" | xargs docker rm\n"]]
      ]      
  , "   ```"
  , ""
  , "2. Remove images"
  , ""
  , "   ```bash"
  , append
      "script.sh"
      Nothing
      [ nodes ["   # <<", fragId, ">>+=\n"]
      , nodes ["   ", code ["docker images --format \"{{.ID}}\" | xargs docker rmi -f\n"]]
      ]      
  , "   ```"
  , ""
  , "3. Remove volumes"
  , ""
  , "   ```bash"
  , append
      "script.sh"
      Nothing
      [ nodes ["   # <<", fragId, ">>+=\n"]
      , nodes ["   ", code ["docker volume prune\n"]]
      ]      
  , "   ```"
  , ""
  , "4. Remove build cache"
  , ""
  , "   ```bash"
  , append
      "script.sh"
      Nothing
      [ nodes ["   # <<", fragId, ">>+=\n"]
      , nodes ["   ", code ["docker build prune\n"]]
      ]      
  , "   ```"
  ]
