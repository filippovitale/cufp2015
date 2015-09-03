#!/usr/bin/env runhaskell

import Turtle

main = do
    dir  <- pwd
    time <- datefile dir
    echo time             -- This used to be: print time

-- type error: 'time' is of type UTCTime (instead of String)
