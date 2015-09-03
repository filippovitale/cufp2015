#!/usr/bin/env runhaskell
                            -- #!/bin/bash
import Turtle               --
                            --
datePwd = do                -- datePwd() {
    dir    <- pwd           --     DIR=$(pwd)
    result <- datefile dir  --     RESULT=$(date -r $DIR)
    return result           --     echo $RESULT
                            -- }
main = do                   --
    time <- datePwd         -- TIME=$(datePwd)
    print time              -- echo $TIME
