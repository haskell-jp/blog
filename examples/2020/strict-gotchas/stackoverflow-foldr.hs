{-
  GHCRTS=-K100k stack exec runghc -- ./stackoverflow-foldr.hs
  GHCRTS=-K100k stack exec runghc -- --ghc-arg=-XStrict ./stackoverflow-foldr.hs
-}

import Control.Exception
import Data.List

main :: IO ()
main = do
  let size = 5000

  evaluate . length $ foldr (:) [] [1 .. size]
  putStrLn "DONE: foldr 1"

  evaluate . length $ foldr (\x z -> x : z) [] [1 .. size]
  putStrLn "DONE: foldr 2"
