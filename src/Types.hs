{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Types where

import           Data.Array.Accelerate
import           Data.Array.Accelerate.Array.Sugar
import           Data.Array.Accelerate.Control.Lens
import           Data.Array.Accelerate.Product

-- A type that holds three Array's with some data inside
data MyAccData = MyAccData {
      firstField  :: Array DIM2 Float
    , secondField :: Array DIM3 Int
    , thirdField  :: Array DIM1 Char
} deriving Show


-- Reprezentation of MyAccData as a tuple
type MyTupRepr = (Array DIM2 Float
                , Array DIM3 Int
                , Array DIM1 Char)


type instance ArrRepr MyAccData = ArrRepr MyTupRepr


instance Arrays MyAccData where
  fromArr (MyAccData a b c) = fromArr (a, b, c)
  toArr p = let (a, b, c) = toArr p in MyAccData a b c
  arrays _ = arrays (undefined :: MyTupRepr)
  flavour _ = ArraysFtuple

instance IsProduct Arrays MyAccData where
  type ProdRepr MyAccData = ProdRepr MyTupRepr
  fromProd p (MyAccData a b c) = fromProd p (a, b, c)
  toProd p tup = let (a, b, c) = toProd p tup in MyAccData a b c
  prod p _ = prod p (undefined :: MyTupRepr)

instance Lift Acc MyAccData where
  type Plain MyAccData = MyTupRepr
  lift (MyAccData a b c) = lift (a, b, c)

toTup :: MyAccData -> MyTupRepr
toTup (MyAccData x y z) = (x, y, z)

first :: Acc (Plain MyAccData) -> Acc (Array DIM2 Float)
first p = let (x, _, _) = unlift p :: (Acc (Array DIM2 Float), Acc (Array DIM3 Int), Acc (Vector Char))
          in x

-- Equivalent types: MyTupRepr ~ Plain MyAccData
second :: Acc MyTupRepr -> Acc (Array DIM3 Int)
second t = t ^. _2

third :: Acc (Plain MyAccData) -> Acc (Array DIM1 Char)
third = view _3     -- (^.) is the infix version of `view`
