{-# LANGUAGE FlexibleContexts #-}

module Attributes
( Attribute(..)
, attribute
, clean
) where

import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec, (<?>), (<|>))
import Text.Parsec.Char (digit)
import qualified Text.Parsec.Combinator as Parsec (choice)


-- | Generic attribute name.
type AttrName = String
-- | Generic attribute value.
type AttrValue = String

-- | Data type representing the possible SVG attributes
-- according to <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute>
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

-- | Parse an attribute.
attribute :: Parsec String () Attribute
attribute = Parsec.spaces *> (Parsec.try (Parsec.choice attributes) <|> anyAttr) <* Parsec.spaces

-- | List of all the attribute parsers.
attributes :: [Parsec String () Attribute]
attributes = map attrParser attrDefs

-- | Construct an attribute parser given its name.
attrParser :: (String, Parsec String () Attribute) -> Parsec String () Attribute
attrParser (name, parser) = do
  Parsec.string name
  Parsec.spaces
  Parsec.char '='
  Parsec.spaces
  Parsec.char '"'
  val <- parser
  Parsec.char '"'
  return $ val

-- | Attributes name and their corresponding parsers.
attrDefs =
  [ ("height", height)
  , ("width", width)
  , ("viewBox", viewBox)
  , ("d", d)
  ]

width :: Parsec String () Attribute
width = Width <$> digits

height :: Parsec String () Attribute
height = Height <$> digits

viewBox :: Parsec String () Attribute
viewBox = ViewBox . tuple4 <$> (map read) <$> (Parsec.many1 Parsec.digit) `Parsec.sepBy` Parsec.spaces

d :: Parsec String () Attribute
d = D <$> Parsec.many (Parsec.noneOf "\"")

-- | Any attribute.
anyAttr :: Parsec String () Attribute
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

-- | Filter a list of attributes to keep only 
-- the ones listed in the 'Attribute' data type.
clean :: [Attribute] -> [Attribute]
clean attrs = filter f attrs
  where 
    f (Attribute (_, _)) = False
    f _ = True