module Immutable.Utils (
  maybeUndefined,
  unsafeShow,
  unsafeEq
  ) where

import Data.Function
import Data.Maybe

foreign import _maybeUndefined
  """
  function _maybeUndefined(just, nothing, v) {
    return v === undefined ? nothing : just(v);
  }
  """
  :: forall v. Fn3 (Maybe v) (v -> Maybe v) v (Maybe v)

foreign import unsafeShow
  """
  function unsafeShow(obj) {
    return String(obj);
  }
  """ :: forall v. v -> String

foreign import _unsafeEq
  """
  var _unsafeEq = require('immutable').is;
  """
  :: forall v. Fn2 v v Boolean

unsafeEq = runFn2 _unsafeEq

maybeUndefined = runFn3 _maybeUndefined Nothing Just

module Immutable.Vector (
  Vector(..),
  empty,
  get
  ) where

import Data.Maybe
import Data.Function
import Immutable.Utils

foreign import data Mod :: *
foreign import mod
  "var mod = require('immutable').Vector"
  :: Mod

foreign import data Vector :: * -> *

foreign import empty
  "var empty = mod.empty();"
  :: forall v. Vector v

foreign import _get
  """
  function _get(maybeUndefined, v, idx) {
    return maybeUndefined(v.get(idx));
  }
  """
  :: forall v. Fn3 (v -> Maybe v) (Vector v) Number (Maybe v)

get = runFn3 _get maybeUndefined

foreign import _map
  """
  function _map(f, v) {
    return v.map(f);
  }
  """
  :: forall a b. Fn2 (a -> b) (Vector a) (Vector b)

map = runFn2 _map

instance showVector :: Show (Vector v) where
  show = unsafeShow

instance eqVector :: Eq (Vector v) where
  (==) = unsafeEq
  (/=) x y = not (x == y)

instance functorVector :: Functor Vector where
  (<$>) = map

module Immutable.Map (
  Map(..),
  empty,
  get,
  set,
  remove,
  update,
  merge
  ) where

import Data.Maybe
import Data.Function
import Immutable.Utils

foreign import data Mod :: *
foreign import mod
  "var mod = require('immutable').Map"
  :: Mod

foreign import data Map :: * -> * -> *

foreign import empty
  "var empty = mod.empty()"
  :: forall k v. Map k v

foreign import _get
  "function _get(maybeUndefined, m, k) { return maybeUndefined(m.get(k)); }"
  :: forall k v. Fn3 (v -> Maybe v) (Map k v) k (Maybe v)

get = runFn3 _get maybeUndefined

foreign import _set
  "function _set(m, k, v) { return m.set(k, v); }"
  :: forall k v. Fn3 (Map k v) k v (Map k v)

set = runFn3 _set

foreign import _remove
  "function _remove(m, k) { return m.remove(k); }"
  :: forall k v. Fn2 (Map k v) k (Map k v)

remove = runFn2 _remove

foreign import _update
  "function _update(m, k, f) { return m.update(k, f); }"
  :: forall k v. Fn3 (Map k v) k (v -> v) (Map k v)

update = runFn3 _update

foreign import _merge
  "function _merge(m, om) { return m.merge(k, f); }"
  :: forall k v. Fn2 (Map k v) (Map k v) (Map k v)

merge = runFn2 _merge

foreign import _map
  """
  function _map(f, m) {
    return m.map(f);
  }
  """
  :: forall k a b. Fn2 (a -> b) (Map k a) (Map k b)

map = runFn2 _map

instance showMap :: Show (Map k v) where
  show = unsafeShow

instance eqMap :: Eq (Map k v) where
  (==) = unsafeEq
  (/=) x y = not (x == y)

instance functorMap :: Functor (Map k) where
  (<$>) = map

module Immutable.Main where

import qualified Immutable.Map as Map
import Debug.Trace

main = do
  let m = Map.set Map.empty "l" 1
  print m
