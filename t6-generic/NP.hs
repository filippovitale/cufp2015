{-# LANGUAGE GADTs, KindSignatures, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NP where

import Data.Proxy
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

eq_NP :: (All Eq xs) => NP I xs -> NP I xs -> Bool
eq_NP Nil         Nil         = True
eq_NP (I x :* xs) (I y :* ys) = x == y && eq_NP xs ys
eq_NP _           _           = error "impossible"

cmap_NP :: (All c xs) => Proxy c -> (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
cmap_NP _ _ Nil      = Nil
cmap_NP p f (x :* xs)  = f x :* cmap_NP p f xs

-- let's tell ghc via a proxy which contraint we mean
-- data Proxy a = Proxy
-- or simply -> import Data.Proxy

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- would work for `Maybe
--- pure_NP :: (forall x . f x) -> NP f xs

class Pure_NP (xs :: [k]) where
  pure_NP' :: (forall x . f x) -> NP f xs

instance Pure_NP '[] where
  pure_NP' _ = Nil

instance (Pure_NP xs) => Pure_NP (x ': xs) where
  pure_NP' x = x :* pure_NP' x

-- pattern matching leaks implementation details
-- instead of pattern matching we could use singleton type

class SingI (xs :: [k]) where
  sing :: Sing xs

data Sing :: [k] -> * where
  SNil  :: Sing '[]
  SCons :: SingI xs => Sing (x ': xs)

instance (SingI xs) => SingI (x ': xs) where
  sing = SCons

-- everything down here will happen at runtime

pure_NP :: SingI xs => (forall x . f x) -> NP f xs
pure_NP x = go sing x
  where
    go :: Sing xs -> (forall x . f x) -> NP f xs
    go SNil  _ = Nil
    go SCons y = y :* pure_NP y

cpure_NP :: (SingI xs, All c xs) => Proxy c -> (forall x . c x => f x) -> NP f xs
cpure_NP = undefined

ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP Nil          Nil       = Nil
ap_NP (Fn f :* fs) (x :* xs) = f x :* ap_NP fs xs
ap_NP _ _ = error "impossible"

newtype (f -.-> g) x = Fn (f x -> g x)

-- SingI constraint everywhere :-(
map_NP' :: SingI xs => (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP' f xs = pure_NP (Fn f) `ap_NP` xs

fn_2 f = (Fn (\x -> Fn (\y -> f x y)))

zipWith_NP :: SingI xs => (forall x . f x -> g x -> h x) -> NP f xs -> NP g xs -> NP h xs
zipWith_NP f xs ys = pure_NP (fn_2 f) `ap_NP` xs `ap_NP` ys

czipWith_NP :: (All c xs, SingI xs)
            => Proxy c
            -> (forall x . c x => f x -> g x -> h x)
            -> NP f xs -> NP g xs -> NP h xs
czipWith_NP p f xs ys = cpure_NP p (fn_2 f) `ap_NP` xs `ap_NP` ys

eq_NP' :: (All Eq xs, SingI xs) => NP I xs -> NP I xs -> Bool
eq_NP' xs ys =
    and
  $ collapse_NP
  $ czipWith_NP (Proxy :: Proxy Eq) (\ (I x) (I y) -> K (x == y)) xs ys

collapse_NP :: NP (K a) xs -> [a]
collapse_NP Nil         = []
collapse_NP (K x :* xs) = x : collapse_NP xs
