{-# LANGUAGE ScopedTypeVariables #-}

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

data Test = Test Int Int deriving Show

instance Storable Test where
  sizeOf _ = sizeOf (1 :: Int) * 2
  alignment _ = 8
  peek = error "This should not be called in this program"
  poke = error "This should not be called in this program"

main :: IO ()
main = alloca $ \(_ :: Ptr Test) -> putStrLn "This won't be printed when Strict is enabled"
