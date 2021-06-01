{-# LANGUAGE CPP #-}

module Data.OpenUnion
    ( Union
    , (@>)
    , inject
    , reinterpret
    , restrict
    , typesExhausted
    )
where

import Control.Exception
import Data.Dynamic

#if MIN_VERSION_base(4,10,0)
import Data.Proxy
import Data.Typeable
#endif

import Prelude
import TypeFun.Data.List (Delete, Elem, SubList, (:++:))
import qualified Data.Aeson

newtype Union (s :: [*]) = Union Dynamic

instance Show (Union '[]) where
    show = typesExhausted

instance (Typeable a, Show a, Show (Union (Delete a as))) => Show (Union (a : as)) where
    show x = case restrict @ a x of
        Left x -> show x
        Right x -> show x

instance Eq (Union '[]) where
    x == _ = typesExhausted x

instance (Typeable a, Eq a, Eq (Union (Delete a as))) => Eq (Union (a : as)) where
    x == y =
        case (restrict @ a x, restrict @ a y) of
            (Right x, Right y) -> y == y
            (Left x, Left y) -> x == y
            _ -> False

instance Ord (Union '[]) where
    compare x _ = typesExhausted x

instance (Typeable a, Ord a, Ord (Union (Delete a as))) => Ord (Union (a ': as)) where
    compare x y =
        case (restrict @ a x, restrict @ a y) of
            (Right x, Right y) -> compare x y
            (Left x, Left y) -> compare x y
            (Right _, Left _) -> GT
            (Left _, Right _) -> LT

instance Data.Aeson.ToJSON (Union '[]) where
    toJSON = typesExhausted

instance (Typeable a, Data.Aeson.ToJSON a, Data.Aeson.ToJSON (Union (TypeFun.Data.List.Delete a b))) => Data.Aeson.ToJSON (Union (a : b)) where
    toJSON x =
        case restrict @ a x of
            Right x -> Data.Aeson.toJSON x
            Left x -> Data.Aeson.toJSON x

(@>) :: Typeable a => (a -> b) -> (Union (Delete a s) -> b) -> Union s -> b
r @> l = either l r . restrict
{-# INLINE (@>) #-}

infixr 2 @>

(@!>) :: (Typeable a, Elem a s) => (a -> b) -> (Union (Delete a s) -> b) -> Union s -> b
r @!> l = either l r . restrict
{-# INLINE (@!>) #-}

infixr 2 @!>

inject :: (Typeable a, Elem a s) => a -> Union s
inject = Union . toDyn
{-# INLINE inject #-}

restrict :: Typeable a => Union s -> Either (Union (Delete a s)) a
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

reinterpret :: (SubList s s') => Union s -> Union s'
reinterpret (Union d) = Union d
{-# INLINE reinterpret #-}

typesExhausted :: Union '[] -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}
