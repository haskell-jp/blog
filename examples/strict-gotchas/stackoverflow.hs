-- The result of this example doesn't change whether with or without Strict.
{-
  stack exec ghc -- -O0 -rtsopts -with-rtsopts=-K1k          ./stackoverflow.hs
  stack exec ghc -- -O0 -rtsopts -with-rtsopts=-K1k -XStrict ./stackoverflow.hs
-}

import Control.Exception

main :: IO ()
main = do
  let size = 50000
  evaluate $ foldr (\x z -> x : z) [] [1 .. size]
  putStrLn "DONE: foldr"
  evaluate $ foldl (\z x -> x : z) [] [1 .. size]
  putStrLn "DONE: foldl"
