{-# LANGUAGE OverloadedStrings #-}

module Main where

import Edlib

main :: IO ()
main = do
  let result = align "hello" "world!" defaultAlignConfig
  putStrLn ("edit_distance('hello', 'world!') = " ++ show (resultEditDistance result))
  print result
