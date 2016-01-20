module Elements
( ElementName
, tags
) where

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
  deriving (Show)



tags :: [(String, ElementName)]
tags = [ 
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
