{-# LANGUAGE OverloadedStrings #-}

module Data.GraphViz.HC.DiagramsTH where

import           Language.Haskell.TH

type FunctionName = String
type DiagramLabel = String

mk :: String -> [(FunctionName, DiagramLabel)] -> Q [Dec]
mk fname0 = return . map mkBinding
  where
    mkBinding (fName, label) =
        ValD (VarP (mkName fName))
             (NormalB (AppE (AppE (VarE (mkName fname0)) (LitE (StringL fName)))
                            (LitE (StringL label)))) []

