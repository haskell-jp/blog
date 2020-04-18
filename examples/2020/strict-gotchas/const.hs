dontReferArgs :: a -> b -> a
dontReferArgs = const

referArgs :: a -> b -> a
referArgs x _ = x

main :: IO ()
main = do
  print $ dontReferArgs "dontReferArgs" (undefined :: Int)
  print $ referArgs "referArgs" (undefined :: Int)
