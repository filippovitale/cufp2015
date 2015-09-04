module Example where

import Prelude
import Control.Modad.Eff

-- effect type – fine grained

main :: Eff (console :: CONSOLE, fs :: FS) Unit
