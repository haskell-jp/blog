-- Originally created by @pxfnc
-- https://qiita.com/pxfnc/items/a26bda6d11402daba675

-- Compare
--    runghc where.hs
-- And
--    runghc --ghc-arg=-XStrict where.hs

main :: IO ()
main = print $ div10 0

div10 :: Int -> Int
div10 n
  | n == 0    = 0
  | otherwise = result
 where
  result = 10 `div` n
