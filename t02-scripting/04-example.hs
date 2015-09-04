#!/usr/bin/env runhaskell
                           -- #!/bin/bash
import Turtle              --
main = do                  -- '<-' ==== '=$'
                           -- '<-' is overloaded
                           -- '=' is NOT overloaded (substitution)
    dir  <- pwd            -- DIR=$(pwd)
    -- time <- datefile dir   -- TIME=$(date -r $DIR)
    print dir -- time             -- echo $TIME
