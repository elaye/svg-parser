{-# LANGUAGE FlexibleContexts #-}

module Elements
( ElementName
, tags
, tag
, endTag
, clean
) where

import Data.List (find)

import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec)
import qualified Text.Parsec.Combinator as Parsec (choice)
import Text.Parsec (Parsec, (<|>))

-- | Data type representing the possible SVG tags.
data ElementName = 
  -- A
  A | AltGlyph | AltGlyphDef | AltGlyphItem | Animate | AnimateColor | AnimateMotion | AnimateTransform
  -- B C
  | Circle | ClipPath | ColorProfile | Cursor
  -- D
  | Defs | Desc
  -- E
  | Ellipse
  -- F
  | FeBlend | FeColorMatrix | FeComponentTransfer | FeComposite | FeConvolveMatrix | FeDiffuseLighting
  | FeDisplacementMap | FeDistantLight | FeFlood | FeFuncA | FeFuncB | FeFuncG | FeFuncR | FeGaussianBlur
  | FeImage | FeMerge | FeMergeNode | FeMorphology | FeOffset | FePointLight | FeSpecularLighting | FeSpotLight
  | FeTile | FeTurbulence | Filter | Font | FontFace | FontFaceFormat | FontFaceName | FontFaceSrc
  | FontFaceUri | ForeignObject
  -- G
  | G | Glyph | GlyphRef
  -- H
  | Hkern
  -- I
  | Image
  -- J K L
  | Line
  | LinearGradient
  -- M
  | Marker | Mask | Metadata | MissingGlyph | Mpath
  -- N O P
  | Path | Pattern | Polygon | Polyline
  -- Q R
  | RadialGradient | Rect
  -- S
  | Script | Set | Stop | Style | Svg | Switch | Symbol
  -- T
  | Text | TextPath | Title | Tref | Tspan
  -- U
  | Use
  -- V - Z
  | View | Vkern
  | ElementName String
  deriving (Eq, Show)

-- | Tags name and their corresponding data constructor.
tagsDef :: [(String, ElementName)]
tagsDef = [ 
        -- A
        ("A", A), ("altGlyph", AltGlyph), ("altGlyphDef", AltGlyphDef), ("altGlyphItem", AltGlyphItem)
        , ("animate", Animate), ("animateColor", AnimateColor), ("animateMotion", AnimateMotion)
        , ("animateTransform", AnimateTransform)
        -- B C
        , ("circle", Circle), ("clipPath", ClipPath), ("color-profile", ColorProfile), ("cursor", Cursor)
        -- D
        , ("defs", Defs), ("desc", Desc)
        -- E
        , ("ellipse", Ellipse)
        -- F
        , ("feBlend", FeBlend), ("feColorMatrix", FeColorMatrix), ("feComponentTransfer", FeComponentTransfer)
        , ("feComposite", FeComposite), ("feConvolveMatrix", FeConvolveMatrix), ("feDiffuseLighting", FeDiffuseLighting)
        , ("feDisplacementMap", FeDisplacementMap), ("feDistantLight", FeDistantLight), ("feFlood", FeFlood)
        , ("feFuncA", FeFuncA), ("feFuncB", FeFuncB), ("feFuncG", FeFuncG), ("feFuncR", FeFuncR)
        , ("feGaussianBlur", FeGaussianBlur), ("feImage", FeImage), ("feMerge", FeMerge), ("feMergeNode", FeMergeNode)
        , ("feMorphology", FeMorphology), ("feOffset", FeOffset), ("fePointLight", FePointLight)
        , ("feSpecularLighting", FeSpecularLighting), ("feSpotLight", FeSpotLight), ("feTile", FeTile)
        , ("feTurbulence", FeTurbulence), ("filter", Filter), ("font", Font), ("font-face", FontFace)
        , ("font-face-format", FontFaceFormat), ("font-face-name", FontFaceName), ("font-face-src", FontFaceSrc)
        , ("font-face-uri", FontFaceUri), ("foreignObject", ForeignObject)
        -- G
        , ("g", G), ("glyph", Glyph), ("glyphRef", GlyphRef)
        -- H
        , ("hkern", Hkern)
        -- I
        , ("image", Image)
        -- J K L
        , ("line", Line), ("linearGradient", LinearGradient)
        -- M
        , ("marker", Marker), ("mask", Mask), ("metadata", Metadata), ("missing-glyph", MissingGlyph), ("mpath", Mpath)
        -- N O P
        , ("path", Path), ("pattern", Pattern), ("polygon", Polygon), ("polyline", Polyline)
        -- Q R
        , ("radialGradient", RadialGradient), ("rect", Rect)
        -- S
        , ("script", Script), ("set", Set), ("stop", Stop), ("style", Style), ("svg", Svg), ("switch", Switch), ("symbol", Symbol)
        -- T
        , ("text", Text), ("textPath", TextPath), ("title", Title), ("tref", Tref), ("tspan", Tspan)
        -- U
        , ("use", Use)
        -- V - Z
        , ("view", View), ("vkern", Vkern)
        ]

-- | Parse a tag.
tag :: Parsec String () ElementName
tag = Parsec.choice tags <|> anyTag

-- | Parse an end tag given the element name.
endTag tag = Parsec.string "</" *> Parsec.string (toString tag) <* Parsec.char '>'

-- | Convert an 'ElementName' into a string.
toString :: ElementName -> String
toString tag = case (find (\x -> (snd x) == tag) tagsDef) of
  Nothing -> ""
  Just (str, _) -> str

-- | List of all the tag parsers.
tags :: [Parsec String () ElementName]
tags = map parser tagsDef
  where 
    parser (str, cons) = do
      Parsec.try $ Parsec.string str
      return cons

-- | Return 'Nothing' if the provided element name is not listed in the 'ElementName' data type.
clean :: ElementName -> Maybe ElementName
clean (ElementName _) = Nothing
clean el = Just el

-- | Any element.
anyTag :: Parsec String () ElementName
anyTag = do
  name <- Parsec.many1 $ Parsec.noneOf " =\n\r"
  return $ ElementName name