data MyTuple a b = MyTuple a b

main :: IO ()
main = do
  let (a, _) = ("Default tuple", error "Error in default tuple")
  print a

  let MyTuple (b1, _) b2 = MyTuple ("Default tuple in MyTuple1", error "Error in default tuple") "Other value in MyTuple1"
  print b1
  print b2

  let MyTuple c _ = MyTuple "MyTuple2" (error "Error in MyTuple2")
  print c
