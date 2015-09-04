#!/usr/bin/env runhaskell
                            -- #!/bin/bash
import Turtle               --
                            --
datePwd = do
    dir <- pwd
    datefile dir

main = do                   --
    time <- datePwd         -- TIME=$(datePwd)
    print time              -- echo $TIME
