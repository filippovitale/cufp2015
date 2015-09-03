#!/usr/bin/env runhaskell

import Turtle

ppp x = echo (repr x)

main = do
    dir  <- pwd
    time <- datefile dir
    ppp time
