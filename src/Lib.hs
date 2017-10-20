{-# LANGUAGE TypeFamilies #-}
module Lib where

import           Data.Array.Accelerate             as A
import           Data.Array.Accelerate.LLVM.Native (run1)
import           Types                             (MyAccData (..), MyTupRepr,
                                                    first, second, third, toTup)


myData :: MyAccData
myData = MyAccData x y z
  where
    x = fromList (Z :. 2 :. 5) $ fmap sin [0..9] :: Array DIM2 Float
    y = fromList (Z :. 2 :. 2 :. 3) [0..11] :: Array DIM3 Int
    z = fromList (Z :. 10) "loremipsum" :: Array DIM1 Char

-- A calculation that uses MyAccData
-- Possible signatures:
-- Acc (Plain MyAccData) -> Acc MyTupRepr
-- Acc MyTupRepr -> Acc MyTupRepr
-- Acc MyTupRepr -> Acc (Plain MyAccData)
-- All these types are equivalent since
--  type Plain MyAccData = MyTupRepr
-- in the 'instance Lift Acc MyAccData' declaration
weirdCalc :: Acc (Plain MyAccData) -> Acc (Plain MyAccData)
weirdCalc dat = lift (f, s, t)
  where
    f = A.map (A.^(2 :: Exp Int)) $ first dat
    s = A.reshape (lift $ Z :. (1 :: Int) :. (1 :: Int) :. (4 :: Int)) . A.sum $ second dat
    t = A.afst . A.filter (A./= A.constant 'm') $ third dat

someFunc :: IO ()
someFunc = print myData >> putStrLn "\n\n" >> print (run1 weirdCalc $ toTup myData)
