{-# LANGUAGE FlexibleContexts #-}
module SvgParser
    ( SVG(..)
    , svg
    , parse
    , parseFile
    , clean
    , prettyPrint
    ) where

import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec, ParseError)
import Text.Parsec ((<?>))

import Data.Maybe (mapMaybe)
import Control.Applicative

import Attributes (Attribute, attribute)
import qualified Attributes as Attr
import Elements (ElementName)
import qualified Elements as Elem

--data SVG = Element String [Attribute] [SVG]
data SVG = Element ElementName [Attribute] [SVG]
          | SelfClosingTag ElementName [Attribute]
          | Body String
          -- | XMLDecl [Attribute]
          | XMLDecl String
          | Comment String
          deriving (Eq, Show)

-- The body of an element, consumes any leading spaces; would be nice to not have the try here
--elementBody :: Parsec String () Body
elementBody = seol *> (Parsec.try tag <|> text) <* seol

-- End tag, assuming thatg we had a normal, non self-closing tag
--endTag :: String -> Parsec String () String
--endTag str = Parsec.string "</" *> Parsec.string str <* Parsec.char '>'

-- Create a body XML element, from text up to the next tag 
--text :: Parsec String () Body
text = Body <$> Parsec.many1 (Parsec.noneOf "><")

tag :: Parsec String () SVG
tag = do
  Parsec.char '<'
  Parsec.spaces
  name <- Elem.tag
  seol
  attr <- attribute `Parsec.sepBy` seol
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
    Elem.endTag name
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

-- | Open and parse a file.
parseFile :: FilePath -> IO (Either ParseError SVG)
parseFile filename = do
  file <- readFile filename
  return $ parseSource filename file

-- | Parse a string.
parse :: String -> Either ParseError SVG
parse file = parseSource "(source)" file 

-- | Parse a string with a specified source name.
parseSource :: String -> String -> Either ParseError SVG
parseSource name file = Parsec.parse svg name file

-- | Clean the SVG. Remove all the unknown tags and attributes. 
-- Be carefull because it removes all the tags and attributes that
-- are not implemented.
clean :: SVG -> Maybe SVG
clean svg = case svg of
  Element name attrs svgs -> case (Elem.clean name) of
    Nothing -> Nothing
    Just _ -> Just $ Element name (Attr.clean attrs) (mapMaybe clean svgs)
  Body txt -> Just $ Body txt
  SelfClosingTag name attrs -> case (Elem.clean name) of
    Nothing -> Nothing
    Just _ -> Just $ SelfClosingTag name (Attr.clean attrs)
  XMLDecl decl -> Just $ XMLDecl decl
  Comment comment -> Just $ Comment comment

prettyPrint :: SVG -> String
prettyPrint svg = case svg of
  Element name attrs svgs -> (show name) ++ " " ++ (show attrs) ++ "\n\n\t" ++ (unlines $ map prettyPrint svgs)
  Body txt -> show txt
  SelfClosingTag name attrs -> (show name) ++ " " ++ (show attrs)
  XMLDecl decl -> show decl
  Comment comment -> show comment

