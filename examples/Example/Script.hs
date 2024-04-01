{-# language OverloadedStrings #-}
module Example.Script (script) where

import Little

script :: Document
script =
  document
  [ "# A literate Bash script\n"
  , "\n"
  , "This script describes how to clean up `docker` resources.\n"
  , "\n"
  , "1. Remove containers\n"
  , "\n"
  , "   ```bash\n"
  , define
      "script.sh"
      Nothing
      [ nodes ["   # <<", fragId, ">>=\n"]
      , nodes ["   ", code ["docker ps --all --format \"{{.ID}}\" | xargs docker rm\n"]]
      ]      
  , "   ```\n"
  , "\n"
  , "2. Remove images\n"
  , "\n"
  , "   ```bash\n"
  , append
      "script.sh"
      Nothing
      [ nodes ["   # <<", fragId, ">>+=\n"]
      , nodes ["   ", code ["docker images --format \"{{.ID}}\" | xargs docker rmi -f\n"]]
      ]      
  , "   ```\n"
  , "\n"
  , "3. Remove volumes\n"
  , "\n"
  , "   ```bash\n"
  , append
      "script.sh"
      Nothing
      [ nodes ["   # <<", fragId, ">>+=\n"]
      , nodes ["   ", code ["docker volume prune\n"]]
      ]      
  , "   ```\n"
  , "\n"
  , "4. Remove build cache\n"
  , "\n"
  , "   ```bash\n"
  , append
      "script.sh"
      Nothing
      [ nodes ["   # <<", fragId, ">>+=\n"]
      , nodes ["   ", code ["docker build prune\n"]]
      ]      
  , "   ```\n"
  ]
