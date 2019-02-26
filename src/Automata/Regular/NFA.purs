module Automata.Regular.NFA
  ( NFA()
  , ConcatenateStates()
  , UnionStates()
  , accepts
  , nfa
  ) where

import Prelude

import Automata.Combinators as C
import Automata.Epsilon (Epsilon(..))
import Data.Enum as Data.Enum
import Data.Foldable as Data.Foldable
import Data.List (List(..), (:))
import Data.List as Data.List
import Data.Set (Set, difference, empty, fromFoldable, insert, member, singleton, union, unions)
import Data.Set as Data.Set

data NFA state sigma = NFA (Set state)
                           (Set sigma)
                           (state -> Epsilon sigma -> Set state)
                           state
                           (Set state)

instance unionNFA
  :: (Ord sigma, Ord state1, Ord state2)
  => C.Union (NFA state1 sigma)
             (NFA state2 sigma)
             (NFA (UnionStates state1 state2) sigma) where
  union = unionImpl

instance concatenateNFA
  :: (Ord sigma, Ord state1, Ord state2)
  => C.Concatenate (NFA state1 sigma)
                   (NFA state2 sigma)
                   (NFA (ConcatenateStates state1 state2) sigma) where
  concatenate = concatenateImpl

-- | Construct a valid NFA.
nfa :: forall sigma state
    .  Data.Enum.BoundedEnum sigma
    => Data.Enum.BoundedEnum state
    => (state -> Epsilon sigma -> Set state)
    -> state
    -> Set state
    -> NFA state sigma
nfa = NFA (Data.Set.fromFoldable states) (Data.Set.fromFoldable sigma)
  where
    sigma :: Array sigma
    sigma = Data.Enum.enumFromTo bottom top
    states :: Array state
    states = Data.Enum.enumFromTo bottom top

accepts :: forall sigma state
        .  Ord sigma
        => Ord state
        => NFA state sigma
        -> List (Epsilon sigma)
        -> Boolean
accepts (NFA _ _ d q0 f) inputs = go (singleton q0) (intersperse Epsilon inputs)
  where
    go :: Set _ -> List _ -> Boolean
    go qs Nil          = qs `intersects` f
    go qs (Epsilon:ss) = go (unions (qs : (flip d Epsilon <$> Data.List.fromFoldable qs))) ss
    go qs (s:ss)       = go (unions $ Data.Set.map (flip d s) qs) ss

unionImpl :: forall sigma state1 state2
          .  Ord sigma
          => Ord state1
          => Ord state2
          => NFA state1 sigma
          -> NFA state2 sigma
          -> NFA (UnionStates state1 state2) sigma
unionImpl (NFA s1 s d1 q1 f1) (NFA s2 _ d2 q2 f2) =
  NFA (U0 `insert` go1 s1 `union` go2 s2)
      s
      delta
      U0
      (go1 f1 `union` go2 f2)
  where
    delta U0     Epsilon = fromFoldable [U1 q1, U2 q2]
    delta U0     _       = empty
    delta (U1 q) a       = go1 $ d1 q a
    delta (U2 q) a       = go2 $ d2 q a
    go1 = Data.Set.map U1
    go2 = Data.Set.map U2

data UnionStates s1 s2 = U0 | U1 s1 | U2 s2

derive instance eqUnionStates :: (Eq s1, Eq s2) => Eq (UnionStates s1 s2)

derive instance ordUnionStates :: (Ord s1, Ord s2) => Ord (UnionStates s1 s2)

concatenateImpl :: forall sigma state1 state2
                .  Ord sigma
                => Ord state1
                => Ord state2
                => NFA state1 sigma
                -> NFA state2 sigma
                -> NFA (ConcatenateStates state1 state2) sigma
concatenateImpl (NFA s1 s d1 q1 f1) (NFA s2 _ d2 q2 f2) =
  NFA (go1 s1 `union` go2 s2)
      s
      delta
      (C1 q1)
      (go2 f2)
  where
    delta (C1 q) Epsilon
      | q `member` f1 = (C2 q2) `insert` go1 (d1 q Epsilon)
    delta (C1 q) a = go1 $ d1 q a
    delta (C2 q) a = go2 $ d2 q a
    go1 = Data.Set.map C1
    go2 = Data.Set.map C2

data ConcatenateStates s1 s2 = C1 s1 | C2 s2

derive instance eqConcatenateStates :: (Eq s1, Eq s2) => Eq (ConcatenateStates s1 s2)

derive instance ordConcatenateStates :: (Ord s1, Ord s2) => Ord (ConcatenateStates s1 s2)

-- These should be in `purescript-sets` so they can be more efficient.
intersects :: forall v. (Ord v) => Set v -> Set v -> Boolean
intersects s1 s2 = s1 `difference` s2 /= s1

intersperse :: forall a. a -> List a -> List a
intersperse x xs = Data.Foldable.intercalate (pure x) (map pure xs)
