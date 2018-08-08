{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2018 Aug 08 (Wed) 13:51:57 by Harold Carr.
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GraphViz.HC.Util where

import           Control.Monad                          (forM_)
import           Data.GraphViz
import           Data.GraphViz.Attributes.Colors.Brewer
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Monadic
import qualified Data.Text.Lazy                         as L
import           Data.Word
import           System.FilePath

------------------------------------------------------------------------------
-- Colors

pastel28 :: Word8 -> Attribute
pastel28 n = Color (toColorList [toColor (BC (BScheme Pastel2 8) n)])

-- http://www.colorcombos.com/color-schemes/2025/ColorCombo2025.html
colorCombo2025CL :: Word8 -> ColorList
colorCombo2025CL n | n == 1 = c (RGB 127 108 138)
                   | n == 2 = c (RGB 175 177 112)
                   | n == 3 = c (RGB 226 206 179)
                   | n == 4 = c (RGB 172 126 100)
                   | otherwise = c (RGB 172 126 100) -- TODO
 where c rgb = toColorList [rgb]

colorCombo2025 :: Word8 -> Attribute
colorCombo2025 n = Color $ colorCombo2025CL n

------------------------------------------------------------------------------
-- one to many

(-->*)       :: n -> [n] -> Dot n
f -->*   [t]  = f --> t
f -->* (t:ts) = f --> t >> f -->* ts
_ -->*    []  = error "nowhere to go"

------------------------------------------------------------------------------
-- Shapes

uBaseShape             :: [Attribute] -> n -> L.Text -> Dot n
uBaseShape      as n l = node n $ [textLabel l, style filled] ++ as
uFixedSize             :: [Attribute] -> [Attribute]
uFixedSize             = ([FixedSize SetNodeSize] ++)

uDoubleCircle          :: [Attribute] -> n -> L.Text -> Dot n
uDoubleCircle   as     = uBaseShape $ [shape  DoubleCircle,  pastel28 1] ++ as
uDoubleCircle'         ::                n -> L.Text -> Dot n
uDoubleCircle'         = uDoubleCircle  $ uFixedSize [Width 1] -- colorCombo2025 1

uCircle                :: [Attribute] -> n -> L.Text -> Dot n
uCircle         as     = uBaseShape $ [shape  Circle,        pastel28 2] ++ as
uCircle'               ::                n -> L.Text -> Dot n
uCircle'               = uCircle        $ uFixedSize [Width 1] -- colorCombo2025 1

uTriangle              :: [Attribute] -> n -> L.Text -> Dot n
uTriangle       as     = uBaseShape $ [shape  Triangle,      pastel28 3] ++ as
uTriangle'             ::                n -> L.Text -> Dot n
uTriangle'             = uTriangle      $ uFixedSize [Width 1] -- colorCombo2025 1

uStar                  :: [Attribute] -> n -> L.Text -> Dot n
uStar           as     = uBaseShape $ [shape  Star,          pastel28 7] ++ as
uStar'                 ::                n -> L.Text -> Dot n
uStar'                 = uStar          $ uFixedSize [Width 1] -- colorCombo2025 1

uRectangle             :: [Attribute] -> n -> L.Text -> Dot n
uRectangle      as     = uBaseShape $ [shape     BoxShape,   pastel28 5] ++ as
uRectangle'            ::                n -> L.Text -> Dot n
uRectangle'            = uRectangle     $ uFixedSize [Width 1] -- colorCombo2025 3

uDiamond               :: [Attribute] -> n -> L.Text -> Dot n
uDiamond        as     = uBaseShape $ [Shape  DiamondShape,  pastel28 4] ++ as
uDiamond'              ::                n -> L.Text -> Dot n
uDiamond'              = uDiamond       $ uFixedSize [Width 1.5, Height 1.5]

uDoubleOctagon         :: [Attribute] -> n -> L.Text -> Dot n
uDoubleOctagon  as     = uBaseShape $ [Shape  DoubleOctagon, pastel28 6] ++ as
uDoubleOctagon'        ::                n -> L.Text -> Dot n
uDoubleOctagon'        = uDoubleOctagon $ uFixedSize [Width 1.5, Height 1.5]

------------------------------------------------------------------------------
-- ALIASES (STATE/PETRI-NET DIAGRAMS)

startEndClosedState                 :: L.Text -> L.Text -> Dot L.Text
startEndClosedState                 = uDoubleCircle'

state                               :: L.Text -> L.Text -> Dot L.Text
state                               = uCircle'

transition                          :: L.Text -> L.Text -> Dot L.Text
transition                          = uRectangle []

decision                            :: L.Text -> L.Text -> Dot L.Text
decision            n l             = node n [textLabel l, shape DiamondShape, pastel28 6, style filled, FixedSize SetNodeSize, Width 1.5, Height 1.5]

------------------------------------------------------------------------------
-- I/O

doDots :: PrintDotRepr dg n => FilePath -> [(FilePath, dg n)] -> IO ()
doDots dir = doDots' dir Dot

doDots' :: PrintDotRepr dg n => FilePath -> GraphvizCommand -> [(FilePath, dg n)] -> IO ()
doDots' dir command cases = forM_ cases (createImage dir command)

doDots''
  :: PrintDotRepr dg n
  => FilePath
  -> GraphvizCommand
  -> [ ( FilePath, Attribute -> dg n ) ]
  -> [Attribute]
  -> [GraphvizOutput]
  -> IO ()
doDots'' dir command cases attributes outFormats =
  forM_ cases $ \(fp, g) ->
    forM_ attributes $ \a ->
      forM_ outFormats $ \outFormat ->
        createImage' dir command (mk fp a, g a) outFormat
 where
  mk fp a = fp ++ "-" ++ filter (/=' ') (show a)

createImage :: PrintDotRepr dg n => FilePath -> GraphvizCommand -> (FilePath, dg n) -> IO FilePath
createImage dir command (n, g) = createImageInDir command dir n Png g

createImage'
  :: PrintDotRepr dg n => FilePath -> GraphvizCommand -> (FilePath, dg n) -> GraphvizOutput -> IO FilePath
createImage' dir command (n, g) outFormat = createImageInDir command dir n outFormat g

createImageInDir :: PrintDotRepr dg n => GraphvizCommand -> FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir c d n o g = Data.GraphViz.addExtension (runGraphvizCommand c g) o (combine d n)

{-
Usage

main :: IO ()
main = do
    doDots [ ("ex1" , graphToDot ex1Params ex1) ]
    doDots [ ("ex2" , ex2)
           , ("ex3" , ex3)
           , ("ex4" , ex4)
           ]

To produce dot output:

import           Data.GraphViz
createImageInDir Dot "/tmp" "out.dot" Canon     ex2
createImageInDir Dot "/tmp" "out.dot" DotOutput ex2

To produce png from above output:
dot -Tpng out.dot.gv  -o out.dot.gv.png
-}

-- End of file.

