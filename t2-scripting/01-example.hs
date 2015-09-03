#!/usr/bin/env runhaskell           -- #!/bin/bash
                                    --
{-# LANGUAGE OverloadedStrings #-}  -- overload string literals (boilerplate)
                                    --
import Turtle                       --

main :: IO ()                       --
main = echo "Hello, world!"         -- echo Hello, world!
