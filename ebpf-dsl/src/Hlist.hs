{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}



module Hlist where


--imports
import Data.Kind (Type)
import Control.Monad()
import Data.Proxy
import Data.Typeable
-- import GHC.Base (Symbol)
import GHC.TypeLits 



{-

Type definition por heterogeneous list
HNil is empty list
Cons operator :# takes a pair of a symbol("promoted" string) and a type(Int, String...) and adds it to an existing list
Basically, every element of the list represents an atribute of a struct in C. 


Example struct we want to represent:
    typedef struct{
        String Nome;
        Int Idade;
    }Aluno;


It's representation as an heterogeneous list in Haskell:
    [("nome", String), ("Idade", Int)]

-}
data HList (xs :: [(Symbol, Type)]) where 
    HNil :: HList '[]
    (:#) :: Pair s t -> HList xs -> HList ('(s, t) ': xs) 
infixr 5 :#






--Instance of Show for the list

instance Show (HList '[]) where
    show HNil = ""

--Prints the pair of the head and does it recursively for the tail
instance (Show t, Show (HList xs)) => Show (HList ('(s, t) ': xs)) where
    show :: (Show t, Show (HList xs)) => HList ('(s, t) : xs) -> String
    show (x :# xs) = showTuple x ++ "\n" ++ show xs where
        showTuple (MkPair name value) = show (symbolVal name) ++ " = " ++ show value





--Pait type that stores the name of the struct field and it's type 
data Pair (s :: Symbol)(t :: Type) where 
  MkPair :: KnownSymbol s => Proxy s -> t -> Pair s t 

data In (s :: Symbol)(t :: Type)(xs :: [(Symbol, Type)]) where 
  Here :: In s t ('(s , t) ': xs)
  There :: In s t xs -> In s t ( '(s1 , t1) ': xs)






type family Getter (s :: Symbol) (xs :: [(Symbol, Type)]) :: Type where
  Getter s ('(s, t) ': xs) = t
  Getter s ('(s1, t1) ': xs) = Getter s xs

type family Setter (s :: Symbol) (t :: Type) (xs :: [(Symbol, Type)]) :: [(Symbol, Type)] where
  Setter s t '[] = '[]
  Setter s t ('(s, t) ': xs) = xs
  Setter s t ('(s1, t1) ': xs) = Setter s t xs




hLookup :: In s t xs -> HList xs -> t 
hLookup Here ((MkPair _ v) :# xs) = v 
hLookup (There p) (_ :# xs) = hLookup p xs




hSetter :: KnownSymbol s => Proxy s -> t -> HList xs -> HList (Setter s t xs)
hSetter _ _ HNil = HNil
hSetter fieldName newValue ((MkPair name value) :# xs) = if symbolVal fieldName == symbolVal name
                                                    then MkPair name newValue :# xs
                                                    else MkPair name value :# hSetter fieldName newValue xs


hGetter :: KnownSymbol s => Proxy s -> HList xs -> Getter s xs
hGetter s xs = hLookup (findSymbol s xs) xs

findSymbol :: KnownSymbol s => Proxy s -> HList xs -> In s t xs
findSymbol s ((MkPair symbol value) :# xs) = if symbolVal s == symbolVal symbol 
                                                then Here 
                                                else There (findSymbol s xs) 












example3 :: HList '[ '("Nome", String), '("Idade", Int)]
example3 = MkPair (Proxy :: Proxy "Nome") "Bruno" :# MkPair (Proxy :: Proxy "Idade") 20 :# HNil -- Proxy :: (Proxy "nome", "Bruno") :# HNil
