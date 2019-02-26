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
import Data.List (List(..), (:))
import Data.Set (Set, empty, fromFoldable, insert, member, singleton, union, unions)
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
  union (NFA q1 s1 d1 q01 f1) (NFA q2 s2 d2 q02 f2) =
    NFA
      (U0 `insert` go1 q1 `union` go2 q2)
      (s1 `union` s2)
      delta
      U0
      (go1 f1 `union` go2 f2)
    where
      delta U0     Epsilon = fromFoldable [U1 q01, U2 q02]
      delta U0     _       = empty
      delta (U1 q) a       = go1 $ d1 q a
      delta (U2 q) a       = go2 $ d2 q a
      go1 = Data.Set.map U1
      go2 = Data.Set.map U2

instance concatenateNFA
  :: (Ord sigma, Ord state1, Ord state2)
  => C.Concatenate (NFA state1 sigma)
                   (NFA state2 sigma)
                   (NFA (ConcatenateStates state1 state2) sigma) where
  concatenate (NFA q1 s1 d1 q01 f1) (NFA q2 s2 d2 q02 f2) =
    NFA
      (go1 q1 `union` go2 q2)
      (s1 `union` s2)
      delta
      (C1 q01)
      (go2 f2)
    where
      delta (C1 q) Epsilon
        | q `member` f1 = (C2 q02) `insert` go1 (d1 q Epsilon)
      delta (C1 q) a = go1 $ d1 q a
      delta (C2 q) a = go2 $ d2 q a
      go1 = Data.Set.map C1
      go2 = Data.Set.map C2

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
accepts (NFA _ _ d q0 f) = go (singleton q0)
  where
    go :: Set state -> List (Epsilon sigma) -> Boolean
    go qs Nil          = not Data.Set.isEmpty (qs `Data.Set.intersection` f)
    go qs (Epsilon:ss) =
      go (unions $ qs `Data.Set.insert` Data.Set.map (flip d Epsilon) qs) ss
    go qs (s:ss)       = go (unions $ Data.Set.map (flip d s) qs) (Epsilon : ss)

data UnionStates s1 s2 = U0 | U1 s1 | U2 s2

derive instance eqUnionStates :: (Eq s1, Eq s2) => Eq (UnionStates s1 s2)

derive instance ordUnionStates :: (Ord s1, Ord s2) => Ord (UnionStates s1 s2)

data ConcatenateStates s1 s2 = C1 s1 | C2 s2

derive instance eqConcatenateStates :: (Eq s1, Eq s2) => Eq (ConcatenateStates s1 s2)

derive instance ordConcatenateStates :: (Ord s1, Ord s2) => Ord (ConcatenateStates s1 s2)

