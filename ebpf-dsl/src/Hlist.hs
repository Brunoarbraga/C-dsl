{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}


module Hlist where


import GHC.TypeLits 
import Data.Kind (Constraint, Type)
import Data.Proxy

 -- definition of heterogeneous lists 

data HList (ts :: [(Symbol, Type)]) where 
    HNil :: HList '[]
    (:#) :: Pair s t -> HList ts -> HList ('(s, t) ': ts) 
infixr 5 :#

-- show instance for heterogeneous list 

type family ShowAll (ts :: [(Symbol, Type)]) :: Constraint where
    ShowAll '[] = () 
    ShowAll ( '(_ , t) ': ts) = (Show t, ShowAll ts)

instance ShowAll ts => Show (HList ts) where 
  show HNil = "[]"
  show (p :# ps) = show p ++ ":" ++ show ps

-- type level pair, for using with HList 

data Pair (s :: Symbol)(t :: Type) where 
  MkPair :: KnownSymbol s => Proxy s -> t -> Pair s t  

instance Show t => Show (Pair s t) where 
  show (MkPair p v) = "(" ++ symbolVal p ++ ", " ++ show v ++ ")"

-- lookup and update functions on HList 

hLookup :: In s t xs -> HList xs -> t 
hLookup Here ((MkPair _ v) :# _) = v 
hLookup (There p) (_ :# xs) = hLookup p xs

hUpdate :: In s t xs -> t -> HList xs -> HList xs 
hUpdate Here v ((MkPair s _) :# hs) = (MkPair s v) :# hs 
hUpdate (There p) v (h :# hs) = h :# (hUpdate p v hs)


hCreate :: HList '[]
hCreate = HNil

 
hAdd :: KnownSymbol s => Proxy s -> t -> HList xs -> HList ('(s, t) ': xs)
hAdd s t xs = MkPair s t :# xs 



-- typed De Bruijn indexes, we represent names in a contexts by its position 

data In (s :: Symbol)(t :: Type)(xs :: [(Symbol, Type)]) where 
  Here :: In s t ('(s , t) ': xs)
  There :: In s t xs -> In s t ( '(s1 , t1) ': xs)
