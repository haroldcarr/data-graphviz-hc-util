{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2014 Aug 03 (Sun) 11:44:44 by Harold Carr.
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

doubleCircle        :: [Attribute] -> n -> Text -> Dot n
doubleCircle as n l = node n $ [textLabel l, shape DoubleCircle, FixedSize SetNodeSize, style filled] ++ as
doubleCircle'       :: n -> Text -> Dot n
doubleCircle'       = doubleCircle [pastel28 1, Width 1] -- colorCombo2025 1

circle              :: [Attribute] -> n -> Text -> Dot n
circle       as n l = node n $ [textLabel l, shape       Circle, FixedSize SetNodeSize, style filled] ++ as
circle'             :: n -> Text -> Dot n
circle'             = circle       [pastel28 2, Width 1] -- colorCombo2025 1

rectangle           :: [Attribute] -> n -> Text -> Dot n
rectangle    as n l = node n $ [textLabel l, shape     BoxShape,                        style filled] ++ as
rectangle'          :: n -> Text -> Dot n
rectangle'          = rectangle    [pastel28 5, Width 1] -- colorCombo2025 3

decision            :: [Attribute] -> n -> Text -> Dot n
decision     as n l = node n $ [textLabel l, Shape DiamondShape, FixedSize SetNodeSize, style filled] ++ as
decision'           :: n -> Text -> Dot n
decision'           = decision     [pastel28 6,       Width 1.5, Height 1.5]

------------------------------------------------------------------------------
-- I/O

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = forM_ cases createImage

createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
createImage (n, g) = createImageInDir "/tmp" n Png g

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)

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

