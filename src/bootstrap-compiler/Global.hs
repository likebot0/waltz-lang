module Global
    ( module Global
    , module Control.Monad.Freer
    , module Control.Monad.Trans
    , Data.IORef.IORef
    , module Data.OpenUnion
    , module Data.Proxy
    , module GHC.TypeLits
    , module Prelude
    , module TypeFun.Data.List
    ) where

import Prelude hiding (return)
import Control.Monad.Freer (Eff, Member, Members, LastMember, runM, send, sendM)
import Control.Monad.Trans (lift)
import Data.OpenUnion
import Data.Proxy
import GHC.TypeLits (KnownSymbol, Symbol)
import TypeFun.Data.List ((:++:))
import qualified Prelude
import qualified Control.Monad.Freer.Internal
import qualified Data.IORef
import qualified TypeFun.Data.List

class DeclareContext s where
    type family Context s :: *

data UseContext s (a :: *) where
    UseContext :: UseContext s (Context s)

data Raise (r :: *) where
    Raise :: Show a => a -> Raise b

raise :: (Member Raise e, Show a) => a -> Eff e b
raise =
    send . Raise

ref :: forall e a. LastMember IO e => a -> Eff e (Data.IORef.IORef a)
ref = sendM . Data.IORef.newIORef

get :: forall e a. LastMember IO e => Data.IORef.IORef a -> Eff e a
get = sendM . Data.IORef.readIORef

set :: forall e a. LastMember IO e => Data.IORef.IORef a -> a -> Eff e ()
set ref x = sendM $ Data.IORef.writeIORef ref x

useContext :: forall s e. (Member (UseContext s) e) => Eff e (Context s)
useContext =
    send (UseContext @ s)

with :: forall s e a. Eff e (Context s) -> Eff (UseContext s : e) a -> Eff e a
with f m = do
    x <- f

    Control.Monad.Freer.Internal.handleRelay
        pure
        (\UseContext k -> k x)
        m

withRaiseHandler :: forall e a. (forall b. Show b => b -> Eff (Return a : e) a) -> Eff (Raise : e) a -> Eff e a
withRaiseHandler f =
    Control.Monad.Freer.Internal.handleRelay
        pure
        (\(Raise x) _ -> run $ f x)

newtype
    Fun s = Fun (forall e. FunConstraint s e => Input s -> Eff e (Output s))

type
    FunConstraint s e = (Members (UseContext s : Raise : Require s) e, LastMember IO e)

instance DeclareContextFun s => DeclareContext s where
    type instance Context s = Fun s

class DeclareContextFun s where
    type family Input s :: *
    type instance Input s = ()

    type family Output s :: *
    type instance Output s = ()

    type family Require s :: [* -> *]
    type instance Require s = '[]

data Return a (r :: *) where
    Return :: a -> Return a b

call :: forall s e. FunConstraint s e => Input s -> Eff e (Output s)
call x =
    (useContext @ s) >>= \(Fun f) -> f x

fun :: forall s e. (forall e. FunConstraint s e => Input s -> Eff (Return (Output s) : e) (Output s)) -> Eff e (Fun s)
fun f =
    pure (Fun (run . f))

return :: forall e a b. a -> Eff (Return a : e) b
return =
    send . Return

run :: Eff (Return a : e) a -> Eff e a
run =
    Control.Monad.Freer.Internal.handleRelay pure (\(Return x) _ -> pure x)

when p f =
    if p then
        f
    else
        pure ()

infixl 1 |>

(|>) a f = f a
