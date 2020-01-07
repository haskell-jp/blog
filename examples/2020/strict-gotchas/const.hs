dontReferArgs :: a -> a -> a
dontReferArgs = const

referArgs :: a -> a -> a
referArgs x _ = x

main :: IO ()
main = do
  print $ dontReferArgs "dontReferArgs" undefined
  print $ referArgs "referArgs" undefined
