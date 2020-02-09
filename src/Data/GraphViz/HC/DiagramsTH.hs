{-# LANGUAGE OverloadedStrings #-}

module Data.GraphViz.HC.DiagramsTH where

import           Language.Haskell.TH

type FunctionName = String
type DiagramLabel = String
type ToolTip      = String

mk :: String -> [(FunctionName, DiagramLabel)] -> Q [Dec]
mk fname0 = return . map mkBinding
  where
    mkBinding (fName, label) =
        ValD (VarP (mkName fName))
             (NormalB (AppE (AppE (VarE (mkName fname0))
                                  (LitE (StringL fName)))
                            (LitE (StringL label))))
             []

mk3 :: String -> [(FunctionName, DiagramLabel, ToolTip)] -> Q [Dec]
mk3 fname0 = return . map mkBinding
 where
  mkBinding (fName, label, tt) =
        ValD (VarP (mkName fName))
             (NormalB (AppE (AppE (AppE (VarE (mkName fname0))
                                        (LitE (StringL fName)))
                                  (LitE (StringL label)))
                            (LitE (StringL tt))))
             []

