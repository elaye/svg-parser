module Attributes
( Attribute
, attribute
) where

import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec)
import Text.Parsec ((<?>))

type AttrName = String
type AttrValue = String

data Attribute = Width Int
                | Height Int
                | ViewBox (Int, Int, Int, Int)
                | Attribute (AttrName, AttrValue)
                deriving (Show)

attribute :: Parsec String () Attribute
attribute = do
  name <- Parsec.many (Parsec.noneOf "= />")
  Parsec.spaces
  Parsec.char '='
  --value <- quotes Parsec.string
  Parsec.char '"'
  value <- Parsec.many (Parsec.noneOf ['"'])
  Parsec.char '"'
  Parsec.spaces
  return $ mkAttribute (name, value)
  --return $ Attribute (name, value)

mkAttribute :: (String, String) -> Attribute
mkAttribute (name, value) = case name of
  "width" -> Width (read value)
  "height" -> Height (read value)
  "viewBox" -> ViewBox $ tuple4 $ map read . words $ value
  _ -> Attribute (name, value)

tuple4 :: [a] -> (a, a, a, a)
tuple4 [a, b, c, d] = (a, b, c, d)