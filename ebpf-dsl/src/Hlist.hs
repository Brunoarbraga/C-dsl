{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}




module Hlist where

import Data.Kind (Type)
import Control.Monad()
import Data.Proxy
-- import GHC.Base (Symbol)
import GHC.TypeLits 
--import Hlist (Pair(MkPair))




data HList (ts :: [(Symbol, Type)]) where 
    HNil :: HList '[]
    (:#) :: Pair s t -> HList ts -> HList ('(s, t) ': ts) 
infixr 5 :#



instance Show (HList '[]) where
    show HNil = ""

instance (Show t, Show (HList ts)) => Show (HList ('(s, t) ': ts)) where
    show :: (Show t, Show (HList ts)) => HList ('(s, t) : ts) -> String
    show (x :# xs) = showTuple x ++ "\n" ++ show xs where
        showTuple (MkPair name value) = show (symbolVal name) ++ " = " ++ show value


data Pair (s :: Symbol)(t :: Type) where 
  MkPair :: KnownSymbol s => Proxy s -> t -> Pair s t 

data In (s :: Symbol)(t :: Type)(xs :: [(Symbol, Type)]) where 
  Here :: In s t ('(s , t) ': xs)
  There :: In s t xs -> In s t ( '(s1 , t1) ': xs)

-- val.x
--  
-- hLookup :: In s t xs -> HList xs -> t 
-- hLookup Here ((MkPair _ v) :# xs) = v 
-- hLookup (There p) (_ :# xs) = hLookup p xs

example3 :: HList '[ '("nome", String)]
example3 = (MkPair (Proxy :: Proxy "nome") "Bruno") :# HNil -- Proxy :: (Proxy "nome", "Bruno") :# HNil







-- type family All (c :: Type -> Constraint)
--     (ts :: [Type]) :: Constraint where
--         All c '[] = () 
--         All c (t ': ts) = (c t, All c ts)

-- instance All Eq ts => Eq (HList ts) where
--     HNil == HNil = True
--     (x :# xs) == (y :# ys) = x == y && xs == ys

-- instance (All Eq ts, All Ord ts) => Ord (HList ts) where
--     compare HNil HNil = EQ
--     compare (x :# xs) (y :# ys) = 
--         case compare x y of
--             LT -> LT
--             GT -> GT
--             EQ -> compare xs ys