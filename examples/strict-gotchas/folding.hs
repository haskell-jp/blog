{-
  stack build gauge
-}

-- The result of this example doesn't change whether with or without Strict.
--   stack exec ghc -- -O0 .\folding.hs
--   stack exec ghc -- -XStrict -O0 .\folding.hs

-- TODO: 本当はメモリーの消費量を測るべき、なはず...

import Data.List
import Gauge

main :: IO ()
main = do
  let size = 10 ** 5
  defaultMain
    [ bench "foldl"  $ whnf (foldl  (\z x -> x : z) []) [1 .. size]
    , bench "foldr"  $ whnf (foldr  (:)             []) [1 .. size]
    , bench "foldl'" $ whnf (foldl' (\z x -> x : z) []) [1 .. size]
    ]
