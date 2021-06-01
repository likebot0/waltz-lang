{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.OpenUnion
  ( Union,
    (@>),
    liftUnion,
    reUnion,
    flattenUnion,
    restrict,
    typesExhausted,
  )
where

import Control.Exception
import Data.Dynamic

#if MIN_VERSION_base(4,10,0)
import Data.Proxy
import Data.Typeable
#endif

import TypeFun.Data.List (Delete, Elem, SubList, (:++:))
import Prelude

-- | The @Union@ type - the phantom parameter @s@ is a list of types
-- denoting what this @Union@ might contain.
-- The value contained is one of those types.
newtype Union (s :: [*]) = Union Dynamic

instance Show (Union '[]) where
    show = typesExhausted

instance (Typeable a, Show a, Show (Union (Delete a as))) => Show (Union (a : as)) where
    show x = case restrict @ a x of
        Left rest -> show rest
        Right a -> show a

instance Eq (Union '[]) where
    a == _ = typesExhausted a

instance (Typeable a, Eq a, Eq (Union (Delete a as))) => Eq (Union (a : as)) where
    x == y = case (restrict @ a x, restrict @ a y) of
          (Right x, Right y) -> y == y
          (Left x, Left y) -> x == y
          _ -> False

instance Ord (Union '[]) where
    compare a _ = typesExhausted a

instance (Typeable a, Ord a, Ord (Union (Delete a as))) => Ord (Union (a ': as)) where
  compare x y = case (restrict @ a x, restrict @ a y) of
      (Right x, Right y) -> compare x y
      (Left x, Left y) -> compare x y
      (Right _, Left _) -> GT
      (Left _, Right _) -> LT

instance (Exception e) => Exception (Union (e ': '[])) where
  toException u = case restrict u of
    Left (sub :: Union '[]) -> typesExhausted sub
    Right (e :: e) -> toException e
  fromException some = case fromException some of
    Just (e :: e) -> Just (liftUnion e)
    Nothing -> Nothing

instance
  ( Exception e,
    Typeable e,
    Typeable es,
    Typeable e1,
    Exception (Union (Delete e (e1 ': es))),
    SubList (Delete e (e1 ': es)) (e ': e1 ': es)
  ) =>
  Exception (Union (e ': e1 ': es))
  where
  toException u = case restrict u of
    Left (sub :: Union (Delete e (e1 ': es))) -> toException sub
    Right (e :: e) -> toException e

  fromException some = case fromException some of
    Just (e :: e) -> Just (liftUnion e)
    Nothing ->
      let sub :: Maybe (Union (Delete e (e1 ': es)))
          sub = fromException some
       in fmap reUnion sub

type family FlatElems a :: [*] where
  FlatElems '[] = '[]
  FlatElems ((Union s) : ss) = s :++: FlatElems ss
  FlatElems (x : s) = x : FlatElems s

-- general note: try to keep from re-constructing Unions if an existing one
-- can just be type-coerced.

-- | `restrict` in right-fixable style.
(@>) ::
  Typeable a =>
  (a -> b) ->
  (Union (Delete a s) -> b) ->
  Union s ->
  b
r @> l = either l r . restrict

infixr 2 @>

{-# INLINE (@>) #-}

-- | `restrict` in right-fixable style with existance restriction.
(@!>) ::
  (Typeable a, Elem a s) =>
  (a -> b) ->
  (Union (Delete a s) -> b) ->
  Union s ->
  b
r @!> l = either l r . restrict

infixr 2 @!>

{-# INLINE (@!>) #-}

liftUnion :: (Typeable a, Elem a s) => a -> Union s
liftUnion = Union . toDyn
{-# INLINE liftUnion #-}

-- | Narrow down a @Union@.
restrict :: Typeable a => Union s -> Either (Union (Delete a s)) a
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

-- | Generalize a @Union@.
reUnion :: (SubList s s') => Union s -> Union s'
reUnion (Union d) = Union d
{-# INLINE reUnion #-}

-- | Flatten a @Union@.
flattenUnion :: Union s -> Union (FlatElems s)
flattenUnion (Union d) = Union d
{-# INLINE flattenUnion #-}

-- | Use this in places where all the @Union@ed options have been exhausted.
typesExhausted :: Union '[] -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}
