module Automata.Regular.NFA
  ( NFA()
  , NFAError(..)
  , ConcatenateStates()
  , UnionStates()
  , accepts
  , nfa
  ) where

import Prelude

import Automata.Combinators as C
import Automata.Epsilon (Epsilon(..))
import Data.Foldable as Data.Foldable
import Data.List (List(..), (:))
import Data.List as Data.List
import Data.Set (Set, difference, empty, fromFoldable, insert, member, singleton, union, unions)
import Data.Set as Data.Set
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V)

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

data NFAError

instance showNFAError :: Show NFAError where
  show _ = ""

-- | Attempt to construct and validate an NFA.
nfa :: forall sigma state
    .  Ord sigma
    => Ord state
    => Set state
    -> Set sigma
    -> (state -> Epsilon sigma -> Set state)
    -> state
    -> Set state
    -> V (List NFAError) (NFA state sigma)
nfa states sigma d q0 f = validate (NFA states sigma d q0 f)

validate :: forall sigma state
         .  Ord sigma
         => Ord state
         => NFA state sigma
         -> V (List NFAError) (NFA state sigma)
validate = pure

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

instance eqUnionStates :: (Eq s1, Eq s2) => Eq (UnionStates s1 s2) where
  eq U0     U0      = true
  eq (U1 q) (U1 q') = q == q'
  eq (U2 q) (U2 q') = q == q'
  eq _      _       = false

instance ordUnionStates :: (Ord s1, Ord s2) => Ord (UnionStates s1 s2) where
  compare U0     U0      = EQ
  compare U0     _       = LT

  compare (U1 _) (U2 _)  = LT
  compare (U1 q) (U1 q') = compare q q'

  compare (U2 q) (U2 q') = compare q q'

  compare q      q'      = compare q' q

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

instance eqConcatenateStates :: (Eq s1, Eq s2) => Eq (ConcatenateStates s1 s2) where
  eq (C1 q) (C1 q') = q == q'
  eq (C2 q) (C2 q') = q == q'
  eq _      _       = false

instance ordConcatenateStates :: (Ord s1, Ord s2) => Ord (ConcatenateStates s1 s2) where
  compare (C1 _) (C2 _)  = LT
  compare (C1 q) (C1 q') = compare q q'

  compare (C2 q) (C2 q') = compare q q'

  compare q      q'      = compare q' q

-- These should be in `purescript-sets` so they can be more efficient.
product :: forall v1 v2. Ord v1 => Ord v2 => Set v1 -> Set v2 -> Set (Tuple v1 v2)
product s1 s2 = Data.Set.fromFoldable $ Tuple <$> Data.List.fromFoldable s1 <*> Data.List.fromFoldable s2

intersects :: forall v. (Ord v) => Set v -> Set v -> Boolean
intersects s1 s2 = s1 `difference` s2 /= s1

intersperse :: forall a. a -> List a -> List a
intersperse x xs = Data.Foldable.intercalate (pure x) (map pure xs)
