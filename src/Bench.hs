module Bench where

import qualified Problem01 as P01
import Criterion.Main


main :: IO()
main = do
  input01 <- map read . lines <$> readFile "input01" :: IO [Int]
  let tgt = 2020
  defaultMain [
    bgroup "Problem 01 - A" [
         bench "List Comprehension"     (whnf (P01.solListComprehension tgt) input01)
       , bench "Lista BinSearch"        (whnf (P01.solLista tgt) input01)
       , bench "Sequence (Finger Tree)" (whnf (P01.solSeq tgt) input01)
       , bench "Vector"                 (whnf (P01.solVector tgt) input01)
       , bench "Array"                  (whnf (P01.solArray tgt) input01)
       , bench "Map"                    (whnf (P01.solMap tgt) input01)
       , bench "HashMap"                (whnf (P01.solHashMap tgt) input01)
       , bench "IntMap"                 (whnf (P01.solIntMap tgt) input01)
       , bench "Set"                    (whnf (P01.solSet tgt) input01)
       , bench "IntSet"                 (whnf (P01.solIntSet tgt) input01)
       , bench "Seq Border"             (whnf (P01.solSeqBorder tgt) input01)
       ],

    bgroup "Problem 01 - B" [
         bench "List Comprehension"     (whnf (P01.solListComprehension3 tgt) input01)
       , bench "Lista BinSearch"        (whnf (P01.solLista3 tgt) input01)
       , bench "Sequence (Finger Tree)" (whnf (P01.solSeq3 tgt) input01)
       , bench "Vector"                 (whnf (P01.solVector3 tgt) input01)
       , bench "Array"                  (whnf (P01.solArray3 tgt) input01)
       , bench "Map"                    (whnf (P01.solMap3 tgt) input01)
       , bench "HashMap"                (whnf (P01.solHashMap3 tgt) input01)
       , bench "IntMap"                 (whnf (P01.solIntMap3 tgt) input01)
       , bench "Set"                    (whnf (P01.solSet3 tgt) input01)
       , bench "IntSet"                 (whnf (P01.solIntSet3 tgt) input01)
       ]
    ]
