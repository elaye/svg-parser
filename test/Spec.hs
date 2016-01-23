import Test.HUnit

import Text.Parsec.Error (ParseError, errorMessages, messageString)

import qualified SvgParser as SVG
import SvgParser (SVG)
import Attributes (Attribute(..))

parseFile :: String -> IO (Either ParseError SVG)
parseFile filename = do
  file <- readFile filename
  return $ SVG.parse file

cmpToFile :: String -> SVG -> Test
cmpToFile filename svg = TestCase $ do
  res <- parseFile filename
  case res of
    Left err -> assertFailure $ show err
    Right svgFromFile -> assertEqual "Simple square" svg (SVG.clean svgFromFile)

square = SVG.Element "svg" 
  [ Width 300
  , Height 300
  , ViewBox (0, 0, 300, 300)
  ] 
  [ SVG.SelfClosingTag "path" [D "m 50,50 200,0 0,200 -200,0 z"]
  ]

simpleSquare :: Test
simpleSquare = cmpToFile "test/svg/square_simple.svg" square

tests = TestList
  [ simpleSquare
  ]

main :: IO Counts
main = runTestTT tests
