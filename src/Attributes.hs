{-# LANGUAGE FlexibleContexts #-}
module Attributes
( Attribute(..)
, attribute
, clean
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
                deriving (Eq, Show)

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
attribute = Parsec.spaces *> (try (Parsec.choice attributes) <|> anyAttr) <* Parsec.spaces

--betweenQuotes = Parsec.char '"' *> Parsec.many Parsec.anyChar <* Parsec.char '"'

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

width = Width <$> digits

height = Height <$> digits

viewBox = ViewBox . tuple4 <$> (map read) <$> (Parsec.many1 Parsec.digit) `Parsec.sepBy` Parsec.spaces

d = D <$> Parsec.many (Parsec.noneOf "\"")

anyAttr = do
  name <- Parsec.many (Parsec.noneOf "= />")
  Parsec.spaces
  Parsec.char '='
  Parsec.spaces
  Parsec.char '"'
  val <- Parsec.many (Parsec.noneOf ['"'])
  Parsec.char '"'
  return $ Attribute (name, val)

digits :: Parsec String () Int
digits = read <$> Parsec.many1 Parsec.digit

tuple4 :: [a] -> (a, a, a, a)
tuple4 [a, b, c, d] = (a, b, c, d)

clean :: [Attribute] -> [Attribute]
clean attrs = filter f attrs
  where 
    f (Attribute (_, _)) = False
    f _ = True