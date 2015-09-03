{-# LANGUAGE GADTs, KindSignatures, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NP where

import GHC.Exts (Constraint)

-- data List a = Nil | Cons (List a)

-- data List a where
--   Nil  :: List a
--   Cons :: a -> List a -> List a

-- :kind IO
-- :kind []
-- :kind [Int]
-- :kind (,)

-- data List :: * -> * where
  -- Nil  :: List a
  -- Cons :: a -> List a -> List a

-- data HList :: [*] -> * where
--   HNil  :: HList '[]
--   HCons :: x -> HList xs -> HList (x ': xs)
--
-- infixr 5 `HCons`

data NP :: (k -> *) -> [k] -> * where
  Nil  :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x ': xs)

-- how type family looks
type family All (c :: * -> Constraint) (xs :: [*]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

-- how it should looks
-- All :: (* -> Constraint) -> [*] -> Constraint

class (f (g x)) => Compose f g x
instance (f (g x)) => Compose f g x

deriving instance All (Compose Show f) xs => Show (NP f xs)

infixr 5 :*

-- identity type (shorthand)
newtype I a = I a
  deriving Show

newtype K a b = K a
  deriving Show

-- example of rank-2 type
map_NP :: (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP _ Nil        = Nil
map_NP f (x :* xs)  = f x :* map_NP f xs
