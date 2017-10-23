{-# LANGUAGE TypeFamilies #-}
module Lib where

import           Control.Monad                     (forM_)
import           Data.Array.Accelerate             as A
import           Data.Array.Accelerate.LLVM.Native (run, run1)
import           Types                             (MyAccData (..), MyTupRepr,
                                                    first, second, third, toTup)


myData :: [Float] -> [Int] -> String -> MyAccData
myData fl il sl = MyAccData x y z
  where
    x = fromList (Z :. 2 :. 5) $ fmap sin fl :: Array DIM2 Float
    y = fromList (Z :. 2 :. 2 :. 3) il :: Array DIM3 Int
    z = fromList (Z :. 10) sl :: Array DIM1 Char

lis :: Int -> [a] -> [[a]]
lis n s = Prelude.take n s : lis n (Prelude.drop n s)

floatDatas :: [[Float]]
floatDatas = Prelude.take 10 $ lis 10 [0..]

intDatas :: [[Int]]
intDatas = Prelude.take 10 $ lis 12 [0..]

strDatas :: [String]
strDatas = Prelude.take 10 $ lis 10 "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut \
                                    \labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris \
                                    \nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit \
                                    \esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt \
                                    \in culpa qui officia deserunt mollit anim id est laborum."

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

myRun :: MyTupRepr -> MyTupRepr
myRun = run1 weirdCalc

myRun' :: MyTupRepr -> MyTupRepr
myRun' = run . lift

someFunc :: IO ()
someFunc = putStrLn "*** function 'run', compile phase every time it is called.\n\n" >> forM_ (myRun' <$> myDatas) print
        >> putStrLn "\n\n\n*** function 'run1', compile phase only once.\n\n" >> forM_ (myRun <$> myDatas) print
  where
    myDatas = toTup <$> Prelude.zipWith3 myData floatDatas intDatas strDatas
