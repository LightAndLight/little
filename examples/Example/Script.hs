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
      [ line ["   # <<", fragId, ">>="]
      , nodes ["   ", code ["docker ps --all --format \"{{.ID}}\" | xargs docker rm"]]
      ]      
  , "   ```"
  , ""
  , "2. Remove images"
  , ""
  , "   ```bash"
  , append
      "script.sh"
      Nothing
      [ line ["   # <<", fragId, ">>+="]
      , nodes ["   ", code ["docker images --format \"{{.ID}}\" | xargs docker rmi -f"]]
      ]      
  , "   ```"
  , ""
  , "3. Remove volumes"
  , ""
  , "   ```bash"
  , append
      "script.sh"
      Nothing
      [ line ["   # <<", fragId, ">>+="]
      , nodes ["   ", code ["docker volume prune"]]
      ]      
  , "   ```"
  , ""
  , "4. Remove build cache"
  , ""
  , "   ```bash"
  , append
      "script.sh"
      Nothing
      [ line ["   # <<", fragId, ">>+="]
      , nodes ["   ", code ["docker build prune"]]
      ]      
  , "   ```"
  ]
