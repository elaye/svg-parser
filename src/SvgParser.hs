{-# LANGUAGE FlexibleContexts #-}
module SvgParser
    ( SVG(..)
    , svg
    , parse
    , clean
    ) where

import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec)
import Text.Parsec ((<?>))

import Control.Applicative

import Attributes (Attribute, attribute)
import qualified Attributes as Attr
--import Elements (Element, element)

data SVG = Element String [Attribute] [SVG]
          | SelfClosingTag String [Attribute]
          | Body String
          -- | XMLDecl [Attribute]
          | XMLDecl String
          | Comment String
          deriving (Show)

-- The body of an element, consumes any leading spaces; would be nice to not have the try here
--elementBody :: Parsec String () Body
elementBody = seol *> (Parsec.try tag <|> text) <* seol

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
  --name <- Parsec.many (Parsec.noneOf " =")
  Parsec.spaces
  --attr <- Parsec.many attribute
  attr <- attribute `Parsec.sepBy` seol
  --Parsec.spaces
  seol
  close <- Parsec.try (Parsec.string "/>" <|> Parsec.string ">")

  -- trying just the closing string of the tag bought me
  -- an enormous performance boost, enough to make the 
  -- difference between being usable and not!
  if (length close) == 2
  then do
    seol
    return (SelfClosingTag name attr)
  else do 
    elementBody <- Parsec.many elementBody
    endTag name
    Parsec.spaces
    return (Element name attr elementBody)

xmlDecl = do 
  Parsec.string "<?xml" 
  decl <- Parsec.many (Parsec.noneOf "?>") 
  Parsec.string "?>"
  return $ XMLDecl decl

comment = do 
  Parsec.string "<!--" 
  decl <- Parsec.many (Parsec.noneOf "-->") 
  Parsec.string "-->"
  return $ Comment decl

eol = Parsec.skipMany Parsec.endOfLine

seol = Parsec.spaces <|> eol

-- | SVG parser.
svg :: Parsec String () SVG
svg = do
  Parsec.spaces
  --x <- Parsec.many tag
  --decl <- (Parsec.try xmlDecl <|> tag)
  --decl <- XMLDecl <$> xmlDecl
  decl <- xmlDecl
  seol
  comment <- comment
  x <- seol *> tag <* seol
  Parsec.spaces
  return x
  --return decl

--parse :: String -> IO ()
--parse :: String -> Either String SVG
parse file = do
  let res = Parsec.parse svg "(source)" file
  case res of
    Left err -> print err
    Right svg -> do
      print svg
      --print (clean svg)

-- | Clean the SVG. Remove all the unknown tags and attributes. 
-- Be carefull because it removes all the tags and attributes that
-- are not implemented.
clean :: SVG -> SVG
clean svg = case svg of
  Element name attrs svgs -> Element name (Attr.clean attrs) (map clean svgs)
  Body txt -> Body txt
  SelfClosingTag name attrs -> SelfClosingTag name (Attr.clean attrs)
  XMLDecl decl -> XMLDecl decl
  Comment comment -> Comment comment