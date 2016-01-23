module Main where

import qualified SvgParser as SVG (parse, clean)
import SvgParser (SVG)

main :: IO ()
main = do
  svg <- readFile "square_test.svg"
  case (SVG.parse svg) of
    Left err -> printErr (show err)
    Right result -> printSVG result

printSVG :: SVG -> IO ()
printSVG svg = do
  putStrLn "\n-------------------------"
  putStrLn "SVG\n"
  putStrLn (show svg)
  putStrLn "\n-------------------------"
  putStrLn "Cleaned SVG\n"
  putStrLn $ show $ SVG.clean svg
  putStrLn "-------------------------"

printErr :: String -> IO ()
printErr err = do
  putStrLn "\n#########################"
  putStrLn "Error: Couldn't parse SVG"
  putStrLn "-------------------------"
  putStrLn err
  putStrLn "#########################"
