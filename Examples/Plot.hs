{-# LANGUAGE ForeignFunctionInterface, BangPatterns, ScopedTypeVariables, TupleSections, 
             RecordWildCards, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import AI.SVM.Simple
import qualified Data.Vector.Storable as V
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal (Options(CairoOptions))


main = do
    let trainingData = [('r', V.fromList [0,0])
                       ,('r', V.fromList [1,1])
                       ,('b', V.fromList [0,1])
                       ,('b', V.fromList [1,0])
                       ,('i', V.fromList [0.5,0.5::Double])
                        ]
    let (m,svm2) = trainClassifier (C 1) (RBF 4) trainingData
    let plot :: QDiagram Cairo V2 Double Any
        plot = circle 1 # fc green # scale 5
               `atop` 
               (circle 1 # fc green # scale 5 `atop` circle 1 # scale 100 # lineWidth 5) # translate (V2 200 200)
               `atop` 
               (circle 1 # fc green # scale 5 # translate (V2 400 400) )
               `atop` 
               foldl (atop) (circle 1 # scale 1)
               [circle 1 # scale 5 # translate  (V2 (400*x) (400*y)) # fc (color svm2 (x,y))
               | x <- [0,0.025..1], y <- [0,0.025..1]] 
    fst $ renderDia Cairo (CairoOptions ("test.png") (dims2D 400 400) PNG False ) plot
  where
    color svm (x,y) = case classify svm  [x,y] of
                        'r' -> red
                        'b' -> blue
                        'i' -> indigo


between a x b = a <= x && x <= b

