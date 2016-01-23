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

-- | Data type representing an SVG document.
data SVG = Element ElementName [Attribute] [SVG]
          | SelfClosingTag ElementName [Attribute]
          | Body String
          | XMLDecl String
          | Comment String
          deriving (Eq, Show)

-- | Parse the children of an element.
elementBody :: Parsec String () SVG
elementBody = seol *> (Parsec.try element <|> text) <* seol

-- | Parse the body of an element.
text :: Parsec String () SVG
text = Body <$> Parsec.many1 (Parsec.noneOf "><")

-- | SVG element parser.
element :: Parsec String () SVG
element = do
  Parsec.char '<'
  Parsec.spaces
  name <- Elem.tag
  seol
  attr <- attribute `Parsec.sepBy` seol
  seol
  close <- Parsec.try (Parsec.string "/>" <|> Parsec.string ">")

  if (length close) == 2
  then do
    seol
    return (SelfClosingTag name attr)
  else do 
    elementBody <- Parsec.many elementBody
    Elem.endTag name
    Parsec.spaces
    return (Element name attr elementBody)

-- | XML declaration parser.
xmlDecl = do 
  Parsec.string "<?xml" 
  decl <- Parsec.many (Parsec.noneOf "?>") 
  Parsec.string "?>"
  return $ XMLDecl decl

-- | Comment parser.
comment = do 
  Parsec.string "<!--" 
  decl <- Parsec.many (Parsec.noneOf "-->") 
  Parsec.string "-->"
  return $ Comment decl

-- | Skip many end of lines.
eol = Parsec.skipMany Parsec.endOfLine

-- | Skip many spaces or end of lines.
seol = Parsec.spaces <|> eol

-- | SVG parser.
svg :: Parsec String () SVG
svg = do
  Parsec.spaces
  decl <- xmlDecl
  seol
  comment <- comment
  x <- seol *> element <* seol
  Parsec.spaces
  return x

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

-- | Pretty print an SVG to the terminal.
prettyPrint :: SVG -> String
prettyPrint svg = case svg of
  Element name attrs svgs -> (show name) ++ " " ++ (show attrs) ++ "\n\n\t" ++ (unlines $ map prettyPrint svgs)
  Body txt -> show txt
  SelfClosingTag name attrs -> (show name) ++ " " ++ (show attrs)
  XMLDecl decl -> show decl
  Comment comment -> show comment