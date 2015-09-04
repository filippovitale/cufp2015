module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception hiding (error)
import Node.FS

import Prelude
import Node.Encoding
import Node.FS.Sync

main = do
  log "Hello sailor!"
  content <- readTextFile UTF8 "bower.json"
  log content
