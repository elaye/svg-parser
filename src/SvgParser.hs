{-# LANGUAGE FlexibleContexts #-}
module SvgParser
    ( parse
    ) where

import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec)
import Text.Parsec ((<?>))

import Control.Applicative

import Attributes (Attribute, attribute)

data SVG = Element String [Attribute] [SVG]
          | SelfClosingTag String [Attribute]
          | Body String
          deriving (Show)

-- The body of an element, consumes any leading spaces; would be nice to not have the try here
--elementBody :: Parsec String () Body
elementBody = Parsec.spaces *> Parsec.try tag <|> text

-- End tag, assuming thatg we had a normal, non self-closing tag
--endTag :: String -> Parsec String () String
endTag str = Parsec.string "</" *> Parsec.string str <* Parsec.char '>'

-- Create a body XML element, from text up to the next tag 
--text :: Parsec String () Body
text = Body <$> Parsec.many1 (Parsec.noneOf "><")

tag :: Parsec String () SVG
tag = do
  Parsec.char '<'
  Parsec.spaces
  name <- Parsec.many (Parsec.letter <|> Parsec.digit)
  Parsec.spaces
  attr <- Parsec.many attribute
  Parsec.spaces
  close <- Parsec.try (Parsec.string "/>" <|> Parsec.string ">")

  -- trying just the closing string of the tag bought me
  -- an enormous performance boost, enough to make the 
  -- difference between being usable and not!
  if (length close) == 2
  then return (SelfClosingTag name attr)
  else do 
    elementBody <- Parsec.many elementBody
    endTag name
    Parsec.spaces
    return (Element name attr elementBody)

--svg :: Parsec String () [SVG]
svg :: Parsec String () SVG
svg = do
  Parsec.spaces
  --x <- Parsec.many tag
  x <- tag
  Parsec.spaces
  return x

parse :: String -> IO ()
parse file = do
  let res = Parsec.parse svg "(source)" file
  case res of
    Left err -> print err
    Right svg -> print svg
