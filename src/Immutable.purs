module Data.Immutable.Vector where

import Prelude hiding (empty)

import Data.Function
import Data.Maybe

foreign import data VectorMod :: *
foreign import data Vector :: * -> *

foreign import mod
  "var mod = require('immutable').Vector;"
  :: VectorMod


--
-- IndexedSeq
--

--
-- If this is a sequence of entries (key-value tuples), it will return a
-- sequence of those entries.
--
-- fromEntries(): Sequence<any, any>;

--
-- Returns the first index at which a given value can be found in the
-- sequence, or -1 if it is not present.
--

foreign import _indexOf
  "function _indexOf(just, nothing, vec, val) { \
  \  var idx = vec.indexOf(val); \
  \  return idx === -1 ? nothing : just(idx); \
  \}"
  :: forall a. Fn4
    (Number -> Maybe Number)
    (Maybe Number)
    (Vector a)
    a
    (Maybe Number)

indexOf vec val = runFn4 _indexOf Just Nothing vec val

--
-- Returns the last index at which a given value can be found in the
-- sequence, or -1 if it is not present.
--

foreign import _lastIndexOf
  "function _lastIndexOf(just, nothing, vec, val) { \
  \  var idx = vec.lastIndexOf(val); \
  \  return idx === -1 ? nothing : just(idx); \
  \}"
  :: forall a. Fn4
    (Number -> Maybe Number)
    (Maybe Number)
    (Vector a)
    a
    (Maybe Number)

lastIndexOf vec val = runFn4 _lastIndexOf Just Nothing vec val

--
-- Returns the first index in the sequence where a value satisfies the
-- provided predicate function. Otherwise -1 is returned.
--

foreign import _findIndex
  "function _findIndex(just, nothing, vec, val) { \
  \  var idx = vec.findIndex(val); \
  \  return idx === -1 ? nothing : just(idx); \
  \}"
  :: forall a.  Fn4
    (Number -> Maybe Number)
    (Maybe Number)
    (Vector a)
    (a -> Number -> Boolean)
    (Maybe Number)

findIndex vec pred = runFn4 _findIndex Just Nothing vec pred

--
-- Returns the last index in the sequence where a value satisfies the
-- provided predicate function. Otherwise -1 is returned.
--

foreign import _findLastIndex
  "function _findLastIndex(just, nothing, vec, val) { \
  \  var idx = vec.findLastIndex(val); \
  \  return idx === -1 ? nothing : just(idx); \
  \}"
  :: forall a.  Fn4
    (Number -> Maybe Number)
    (Maybe Number)
    (Vector a)
    (a -> Number -> Boolean)
    (Maybe Number)

findLastIndex vec pred = runFn4 _findLastIndex Just Nothing vec pred

--
-- Splice returns a new indexed sequence by replacing a region of this sequence
-- with new values. If values are not provided, it only skips the region to
-- be removed.
--
--     Sequence(['a','b','c','d']).splice(1, 2, 'q', 'r', 's')
--     // ['a', 'q', 'r', 's', 'd']
--

foreign import _splice
  "function _splice(vec, index, removeNum, values) { \
  \  var args = [index, removeNum].concat(values); \
  \  return vec.splice.apply(vec, args); \
  \}"
  :: forall a. Fn4 (Vector a) Number Number (Vector a) (Vector a)

splice = runFn4 _splice

--
-- When IndexedSequence is converted to an array, the index keys are
-- maintained. This differs from the behavior of Sequence which
-- simply makes a dense array of all values.
-- @override
--
foreign import toArray
  "function toArray(vec) { \
  \  return vec.toArray(); \
  \}"
  :: forall a. Vector a -> [a]

--
-- This new behavior will iterate through the values and sequences with
-- increasing indices.
-- @override
--
foreign import _concat
  "function _concat(vec1, vec2) { \
  \  return vec1.concat(vec2); \
  \}"
  :: forall a. Fn2 (Vector a) (Vector a) (Vector a)

concat = runFn2 _concat

--
-- This new behavior will not only iterate through the sequence in reverse,
-- but it will also reverse the indices so the last value will report being
-- at index 0. If you wish to preserve the original indices, set
-- maintainIndices to true.
-- @override
--

foreign import reverse
  "function reverse(vec) { \
  \  return vec.reverse(); \
  \}"
  :: forall a. Vector a -> Vector a

--
-- Indexed sequences have a different `filter` behavior, where the filtered
-- values have new indicies incrementing from 0. If you want to preserve the
-- original indicies, set maintainIndices to true.
-- @override
--
foreign import _filter
  "function _filter(vec, pred) { \
  \  return vec.filter(pred); \
  \}"
  :: forall a. Fn2 (Vector a) (a -> Boolean) (Vector a)

filter = runFn2 _filter

--
-- Adds the ability to maintain original indices.
-- @override
--
--slice(start: number, end?: number, maintainIndices?: boolean): IndexedSequence<T>;

foreign import _slice
  "function _slice(vec, from, to) { \
  \  return vec.slice(from, to); \
  \}"
  :: forall a. Fn3 (Vector a) Number Number (Vector a)

slice = runFn3 _slice

foreign import _sliceFrom
  "function _sliceFrom(vec, from) { \
  \  return vec.slice(from); \
  \}"
  :: forall a. Fn2 (Vector a) Number (Vector a)

sliceFrom = runFn2 _sliceFrom

--
-- Has the same altered behavior as `takeWhile`.
-- @override
--

foreign import _take
  "function _take(vec, amount) { \
  \  return vec.take(amount); \
  \}"
  :: forall a. Fn2 (Vector a) Number (Vector a)

take = runFn2 _take

--
-- Has the same altered behavior as `takeWhile`.
-- @override
--
--takeLast(amount: number, maintainIndices?: boolean): IndexedSequence<T>;

foreign import _takeLast
  "function _takeLast(vec, amount) { \
  \  return vec.takeLast(amount); \
  \}"
  :: forall a. Fn2 (Vector a) Number (Vector a)

takeLast = runFn2 _takeLast

--
-- Indexed sequences have a different `takeWhile` behavior. The first
-- value will have an index of 0 and the length of the sequence could be
-- truncated. If you want to preserve the original indicies, set
-- maintainIndices to true.
-- @override
--

foreign import _takeWhile
  "function _takeWhile(vec, pred) { \
  \  return vec.takeWhile(pred); \
  \}"
  :: forall a. Fn2 (Vector a) (a -> Boolean) (Vector a)

takeWhile = runFn2 _takeWhile

--
-- Has the same altered behavior as `takeWhile`.
-- @override
--

foreign import _takeUntil
  "function _takeUntil(vec, pred) { \
  \  return vec.takeUntil(pred); \
  \}"
  :: forall a. Fn2 (Vector a) (a -> Boolean) (Vector a)

takeUntil = runFn2 _takeUntil

--
-- Has the same altered behavior as `skipWhile`.
-- @override
--

foreign import _skip
  "function _skip(vec, amount) { \
  \  return vec.skip(amount); \
  \}"
  :: forall a. Fn2 (Vector a) Number (Vector a)

skip = runFn2 _skip

--
-- Has the same altered behavior as `skipWhile`.
-- @override
--

foreign import _skipLast
  "function _skipLast(vec, amount) { \
  \  return vec.skipLast(amount); \
  \}"
  :: forall a. Fn2 (Vector a) Number (Vector a)

skipLast = runFn2 _skipLast

--
-- Indexed sequences have a different `skipWhile` behavior. The first
-- non-skipped value will have an index of 0. If you want to preserve the
-- original indicies, set maintainIndices to true.
-- @override
--

foreign import _skipWhile
  "function _skipWhile(vec, pred) { \
  \  return vec.skipWhile(pred); \
  \}"
  :: forall a. Fn2 (Vector a) (a -> Boolean) (Vector a)

skipWhile = runFn2 _skipWhile

--
-- Has the same altered behavior as `skipWhile`.
-- @override
--

foreign import _skipUntil
  "function _skipUntil(vec, pred) { \
  \  return vec.skipUntil(pred); \
  \}"
  :: forall a. Fn2 (Vector a) (a -> Boolean) (Vector a)

skipUntil = runFn2 _skipUntil

--
--
-- Indexed sequences have a different `groupBy` behavior. Each group will be
-- a new indexed sequence starting with an index of 0. If you want to preserve
-- the original indicies, set maintainIndices to true.
-- @override
--
--groupBy<G>(
--  grouper: (value?: T, index?: number, seq?: IndexedSequence<T>) => G,
--  thisArg?: any,
--  maintainIndices?: boolean
--): Map<G, any/*IndexedSequence<T>*/>; // Bug: exposing this causes the type checker to implode.
--

--
-- Returns an IndexedSequence
-- @override
--

foreign import _map
  "function _map(vec, func) { \
  \  return vec.map(func); \
  \}"
  :: forall a b. Fn2 (Vector a) (a -> b) (Vector b)

map = runFn2 _map

--
-- Returns an IndexedSequence
-- @override
--

foreign import cached
  "function cached(vec) { return vec.cacheResult(); }"
  :: forall a. Vector a -> Vector a


foreign import _empty
  "function _empty(mod) { return mod.empty(); }"
  :: forall a. VectorMod -> (Vector a)

empty = _empty mod

foreign import _fromArray
  "function _fromArray(mod) { \
  \  return function fromArray(array) { \
  \    return mod.from(array); \
  \  } \
  \}"
  :: forall a. VectorMod -> [a] -> Vector a

fromArray = _fromArray mod

foreign import _set
  "function _set(mod) { \
  \  return function set(vec, index, value) { \
  \    return vec.set(index, value); \
  \  } \
  \}"
  :: forall a. VectorMod -> Fn3 (Vector a) Number a (Vector a)

set = runFn3 (_set mod)

foreign import _remove
  "function _remove(mod) { \
  \  return function remove(vec, index) { \
  \    return vec.delete(index); \
  \  } \
  \}"
  :: forall a. VectorMod -> Fn2 (Vector a) Number (Vector a)

remove = runFn2 (_remove mod)

foreign import _showVector
  "function _showVector(vec) { return vec.toString(); }"
  :: forall a. Vector a -> String

instance showVector :: Show (Vector a) where
  show = _showVector

import Debug.Trace

main = do
  let v = set empty 0 2
  let v2 = set v 1 4
  let x = set empty 0 "a"
  print v2
  print x
