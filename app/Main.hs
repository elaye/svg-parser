module Main where

import SvgParser (parse)

main :: IO ()
main = do
  svg <- readFile "square_test.svg"
  parse svg
