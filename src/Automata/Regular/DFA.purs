module Automata.Regular.DFA
  ( DFA()
  , accepts
  , dfa
  ) where

import Prelude

import Automata.Combinators as C
import Data.Enum as Data.Enum
import Data.List (List(..), (:))
import Data.List as Data.List
import Data.Set (Set, member, union)
import Data.Set as Data.Set
import Data.Tuple (Tuple(..))

-- | A Deterministic Finite Automaton (DFA) is a 5-tuple (Q, Σ, δ, q₀, F)
-- | We only expose the type.
-- | We leave the smart constructor to validate a potential DFA.
data DFA state sigma = DFA (Set state)               -- Q  The set of states.
                           (Set sigma)               -- Σ  The alphabet of transitions.
                           (state -> sigma -> state) -- δ  The transition function.
                           state                     -- q₀ The initial state.
                           (Set state)               -- F  The set of accepting states.

instance unionDFA
  :: (Ord sigma, Ord state1, Ord state2)
  => C.Union (DFA state1 sigma)
             (DFA state2 sigma)
             (DFA (Tuple state1 state2) sigma) where
  union = unionImpl

-- | A DFA accepts a string in the alphabet
-- | if the last transition puts the DFA in one of the accepting states.
accepts :: forall sigma state
        .  Ord sigma
        => Ord state
        => DFA state sigma
        -> List sigma
        -> Boolean
accepts (DFA _ _ d q0 f) inputs = go q0 inputs
  where
    go q Nil    = q `member` f
    go q (s:ss) = go (d q s) ss

-- | Construct a valid DFA.
dfa :: forall sigma state
    .  Data.Enum.BoundedEnum sigma
    => Data.Enum.BoundedEnum state
    => (state -> sigma -> state)
    -> state
    -> Set state
    -> DFA state sigma
dfa = DFA (Data.Set.fromFoldable states) (Data.Set.fromFoldable sigma)
  where
    sigma :: Array sigma
    sigma = Data.Enum.enumFromTo bottom top
    states :: Array state
    states = Data.Enum.enumFromTo bottom top

unionImpl :: forall sigma state1 state2
          .  Ord sigma
          => Ord state1
          => Ord state2
          => DFA state1 sigma
          -> DFA state2 sigma
          -> DFA (Tuple state1 state2) sigma
unionImpl (DFA s1 s d1 q1 f1) (DFA s2 _ d2 q2 f2) =
  DFA (product s1 s2)
      s
      (\(Tuple r1 r2) a -> Tuple (d1 r1 a) (d2 r2 a))
      (Tuple q1 q2)
      (product f1 s2 `union` product s1 f2)

-- These should be in `purescript-sets` so they can be more efficient.
product :: forall v1 v2. Ord v1 => Ord v2 => Set v1 -> Set v2 -> Set (Tuple v1 v2)
product s1 s2 = Data.Set.fromFoldable $ Tuple <$> Data.List.fromFoldable s1 <*> Data.List.fromFoldable s2
