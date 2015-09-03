#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
  echo "Line 1"
  echo "Line 2"

  -- main = do
  --     { echo "Line 1"
  --     ; echo "Line 2"
  --     }
  -- main = do {
  --     echo "Line 1";
  --     echo "Line 2";
  --     }
  -- main = do { echo "Line1"; echo "Line2" }
