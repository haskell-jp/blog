import Foreign.Storable
import Foreign.Marshal.Alloc

data Test = Test Int Int deriving Show

-- WIP: できればStorableを使いたいが、実装を作るのが大変そうなのでオリジナルの型クラスを作った方がいいかも
--      本質的には、Strictが有効になってないモジュールで定義された、関数を受け取る関数がどのように
--      ユーザーが渡した関数を使うかわからない、という問題の型クラス（implicit params）版なので、
--      undefinedを渡す、なんて極端な例じゃなくても面白い例が作れそう。
instance Storable Test where
  sizeOf _ = sizeOf (1 :: Int) * 2
  alignment _ = 8
  peek = error "This should not used in this program"
  poke = error "This should not used in this program"

main :: IO ()
main = alloca $ \ptr -> do
  poke ptr $ Test 1 2
  print =<< peek ptr
