{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2014 Aug 10 (Sun) 16:08:05 by Harold Carr.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.GraphViz.HC.Util where

import           Control.Monad                          (forM_)
import           Data.GraphViz
import           Data.GraphViz.Attributes.Colors.Brewer
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                         as L
import           Data.Word
import           System.FilePath

------------------------------------------------------------------------------
-- Colors

pastel28 :: Word8 -> Attribute
pastel28 n = Color (toColorList [toColor (BC (BScheme Pastel2 8) n)])

-- http://www.colorcombos.com/color-schemes/2025/ColorCombo2025.html
colorCombo2025CL :: Word8 -> ColorList
colorCombo2025CL n | n == 1 = c $ (RGB 127 108 138)
                   | n == 2 = c $ (RGB 175 177 112)
                   | n == 3 = c $ (RGB 226 206 179)
                   | n == 4 = c $ (RGB 172 126 100)
 where c rgb = toColorList [rgb]

colorCombo2025 :: Word8 -> Attribute
colorCombo2025 n = Color $ colorCombo2025CL n

------------------------------------------------------------------------------
-- Shapes

uBaseShape             :: [Attribute] -> n -> Text -> Dot n
uBaseShape      as n l = node n $ [textLabel l, style filled] ++ as
uFixedSize             :: [Attribute] -> [Attribute]
uFixedSize             = ([FixedSize SetNodeSize] ++)

uDoubleCircle          :: [Attribute] -> n -> Text -> Dot n
uDoubleCircle   as     = uBaseShape $ [shape  DoubleCircle,  pastel28 1] ++ as
uDoubleCircle'         ::                n -> Text -> Dot n
uDoubleCircle'         = uDoubleCircle  $ uFixedSize [Width 1] -- colorCombo2025 1

uCircle                :: [Attribute] -> n -> Text -> Dot n
uCircle         as     = uBaseShape $ [shape  Circle,        pastel28 2] ++ as
uCircle'               ::                n -> Text -> Dot n
uCircle'               = uCircle        $ uFixedSize [Width 1] -- colorCombo2025 1

uTriangle              :: [Attribute] -> n -> Text -> Dot n
uTriangle       as     = uBaseShape $ [shape  Triangle,      pastel28 3] ++ as
uTriangle'             ::                n -> Text -> Dot n
uTriangle'             = uTriangle      $ uFixedSize [Width 1] -- colorCombo2025 1

uStar                  :: [Attribute] -> n -> Text -> Dot n
uStar           as     = uBaseShape $ [shape  Star,          pastel28 7] ++ as
uStar'                 ::                n -> Text -> Dot n
uStar'                 = uStar          $ uFixedSize [Width 1] -- colorCombo2025 1

uRectangle             :: [Attribute] -> n -> Text -> Dot n
uRectangle      as     = uBaseShape $ [shape     BoxShape,   pastel28 5] ++ as
uRectangle'            ::                n -> Text -> Dot n
uRectangle'            = uRectangle     $ uFixedSize [Width 1] -- colorCombo2025 3

uDiamond               :: [Attribute] -> n -> Text -> Dot n
uDiamond        as     = uBaseShape $ [Shape  DiamondShape,  pastel28 4] ++ as
uDiamond'              ::                n -> Text -> Dot n
uDiamond'              = uDiamond       $ uFixedSize [Width 1.5, Height 1.5]

uDoubleOctagon         :: [Attribute] -> n -> Text -> Dot n
uDoubleOctagon  as     = uBaseShape $ [Shape  DoubleOctagon, pastel28 6] ++ as
uDoubleOctagon'        ::                n -> Text -> Dot n
uDoubleOctagon'        = uDoubleOctagon $ uFixedSize [Width 1.5, Height 1.5]

------------------------------------------------------------------------------
-- I/O

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = doDots' Dot cases

doDots' :: PrintDotRepr dg n => GraphvizCommand -> [(FilePath, dg n)] -> IO ()
doDots' command cases = forM_ cases (createImage command)

createImage :: PrintDotRepr dg n => GraphvizCommand -> (FilePath, dg n) -> IO FilePath
createImage command (n, g) = createImageInDir command "/tmp" n Png g

createImageInDir :: PrintDotRepr dg n => GraphvizCommand -> FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir c d n o g = Data.GraphViz.addExtension (runGraphvizCommand c g) o (combine d n)

{-
Usage
#+BEGIN_SRC haskell
main :: IO ()
main = do
    doDots [ ("ex1" , graphToDot ex1Params ex1) ]
    doDots [ ("ex2" , ex2)
           , ("ex3" , ex3)
           , ("ex4" , ex4)
           ]
-}

-- End of file.

