{-# LANGUAGE FlexibleContexts #-}
module Attributes
( Attribute
, attribute
) where

import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec, between, (<?>), (<|>), try)
import Text.Parsec.Char (digit)
import qualified Text.Parsec.Combinator as Parsec (choice)


type AttrName = String
type AttrValue = String

data Attribute = Width Int
                | Height Int
                | ViewBox (Int, Int, Int, Int)
                | D String
                | Attribute (AttrName, AttrValue)
                deriving (Show)

data D = MoveToAbs [(Float, Float)]
        | MoveToRel [(Float, Float)]
        | LineToAbs [(Float, Float)]
        | LineToRel [(Float, Float)]
        | CurveToQuadAbs [((Float, Float), (Float, Float))]
        | CurveToQuadRel [((Float, Float), (Float, Float))]
        | CurveToCubAbs [((Float, Float), (Float, Float))]
        | CurveToCubRel [((Float, Float), (Float, Float), (Float, Float))]
        | ArcTo String
        | ClosePath

--attribute :: Parsec String () Attribute
--attribute = do
--  name <- Parsec.many (Parsec.noneOf "= />")
--  Parsec.spaces
--  Parsec.char '='
--  --value <- quotes Parsec.string
--  Parsec.char '"'
--  value <- Parsec.many (Parsec.noneOf ['"'])
--  Parsec.char '"'
--  Parsec.spaces
--  return $ mkAttribute (name, value)
--  --return $ Attribute (name, value)

--attribute :: Parsec String () Attribute
attribute = Parsec.spaces *> Parsec.choice attributes <* Parsec.spaces

--attribute :: Parsec String () Attribute
--attribute = do
--  name <- Parsec.many (Parsec.noneOf "= />")
--  Parsec.spaces
--  value <- Parsec.char '=' *> betweenQuotes
--  return (name, value)

--betweenQuotes = Parsec.char '"' *> Parsec.many Parsec.anyChar <* Parsec.char '"'

--attributes :: [Parsec String () Attribute]
--attributes = 
--  [ width
--  , height
--  , viewBox
--  , d
--  , anyAttr
--  ]

--attributes = map mkParser attrDefs : anyAttr
attributes :: [Parsec String () Attribute]
attributes = map attrParser attrDefs

attrParser (name, parser) = do
  Parsec.string name
  Parsec.spaces
  Parsec.char '='
  Parsec.spaces
  Parsec.char '"'
  val <- parser
  Parsec.char '"'
  return $ val

attrDefs =
  [ ("height", height)
  , ("width", width)
  , ("viewBox", viewBox)
  , ("d", d)
  ]

digits :: Parsec String () Int
digits = read <$> Parsec.many1 Parsec.digit

--attributes = 
--  [ ("width", Width)
--  , ("height", Height)
--  , ("viewBox", (,,,))
--  ]

--anyAttr = do
--  name <- Parsec.many (Parsec.noneOf "= />")
--  Parsec.spaces
--  Parsec.char '='
--  Parsec.spaces
--  Parsec.char '"'
--  val <- Parsec.many Parsec.anyChar
--  Parsec.char '"'
--  return $ Attribute (name, val)

--width :: Parsec String () Attribute
--width = do
--  Parsec.string "width"
--  Parsec.spaces
--  Parsec.char '='
--  Parsec.spaces
--  Parsec.char '"'
--  val <- digits
--  Parsec.char '"'
--  return $ Width val
width = Width <$> digits

--height :: Parsec String () Attribute
--height = do
--  Parsec.string "height"
--  Parsec.spaces
--  Parsec.char '='
--  Parsec.spaces
--  Parsec.char '"'
--  val <- digits
--  Parsec.char '"'
--  return $ Height val
height = Height <$> digits

--viewBox :: Parsec String () (AttrName, AttrValue)
--viewBox = do
--  Parsec.string "viewBox"
--  Parsec.spaces
--  Parsec.char '='
--  Parsec.spaces
--  Parsec.char '"'
--  --val <- digits
--  val <- tuple4 . map read <$> (Parsec.many1 Parsec.digit) `Parsec.sepBy` Parsec.spaces
--  Parsec.char '"'
--  return $ ViewBox val

viewBox = ViewBox . tuple4 <$> (map read) <$> (Parsec.many1 Parsec.digit) `Parsec.sepBy` Parsec.spaces

--d = do
--  Parsec.string "d"
--  Parsec.spaces
--  Parsec.char '='
--  Parsec.spaces
--  Parsec.char '"'
--  --val <- Parsec.many Parsec.anyChar
--  val <- Parsec.many $ Parsec.noneOf "\""
--  Parsec.char '"'
--  return $ D val
d = D <$> Parsec.many (Parsec.noneOf "\"")


--quotes :: Parsec String () String
--quotes = Parsec.between (Parsec.symbol "\"") (Parsec.symbol "\"")

--mkAttribute :: (String, String) -> Attribute
--mkAttribute (name, value) = case name of
--  "width" -> Width (read value)
--  "height" -> Height (read value)
--  "viewBox" -> ViewBox $ tuple4 $ map read . words $ value
--  "d" -> D value
--  _ -> Attribute (name, value)

tuple4 :: [a] -> (a, a, a, a)
tuple4 [a, b, c, d] = (a, b, c, d)