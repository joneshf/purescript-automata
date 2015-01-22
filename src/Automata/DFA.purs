module Automata.DFA
  ( DFA()
  , DFAError(..)
  , accepts
  , dfa
  , union
  ) where

  import Data.Foldable (all)
  import Data.Tuple (Tuple(..))
  import Data.Validation (V(), invalid)

  import qualified Data.Set as S

  -- | A Deterministic Finite Automaton (DFA) is a 5-tuple (Q, Σ, δ, q₀, F)
  -- | We only expose the type.
  -- | We leave the smart constructor to validate a potential DFA.
  data DFA state sigma = DFA (S.Set state)               -- Q  The set of states.
                             (S.Set sigma)               -- Σ  The alphabet of transitions.
                             (state -> sigma -> state) -- δ  The transition function.
                             state                     -- q₀ The initial state.
                             (S.Set state)               -- F  The set of accepting states.

  -- | A DFA can be invalid in one of two conditions.
  -- | TODO: It'd be nice to validate that the transition function is total
  -- |       Maybe with `purescript-totally`
  data DFAError = StartState   -- q₀ ∉ Q
                | AcceptStates -- F  ⊈ Q

  instance showDFAError :: Show DFAError where
    show StartState   = "Start state is not a member of the set of states."
    show AcceptStates = "Accept states are not a subset of the set of states."

  -- | A DFA accepts a string in the alphabet
  -- | if the last transition puts the DFA in one of the accepting states.
  accepts :: forall sigma state
          .  (Ord sigma, Ord state)
          => DFA state sigma
          -> [sigma]
          -> Boolean
  accepts (DFA _ _ d q0 f) inputs = go q0 inputs
    where
      go q []     = q `S.member` f
      go q (s:ss) = go (d q s) ss

  -- | Attempt to construct and validate a DFA.
  dfa :: forall sigma state
      .  (Ord sigma, Ord state)
      => S.Set state
      -> S.Set sigma
      -> (state -> sigma -> state)
      -> state
      -> S.Set state
      -> V [DFAError] (DFA state sigma)
  dfa states sigma d q0 f = validate (DFA states sigma d q0 f)

  validate :: forall sigma state
           .  (Ord sigma, Ord state)
           => DFA state sigma
           -> V [DFAError] (DFA state sigma)
  validate dfa = const <$> validateStart dfa <*> validateAccept dfa

  validateStart :: forall sigma state
                .  (Ord sigma, Ord state)
                => DFA state sigma
                -> V [DFAError] (DFA state sigma)
  validateStart dfa@(DFA states _ _ q0 _)
    | q0 `S.member` states = pure dfa
    | otherwise          = invalid [StartState]

  validateAccept :: forall sigma state
                 .  (Ord sigma, Ord state)
                 => DFA state sigma
                 -> V [DFAError] (DFA state sigma)
  validateAccept dfa@(DFA states _ _ _ f)
    | f `isSubsetOf` states = pure dfa
    | otherwise             = invalid [AcceptStates]

  union :: forall sigma state1 state2
        .  (Ord sigma, Ord state1, Ord state2)
        => DFA state1 sigma
        -> DFA state2 sigma
        -> DFA (Tuple state1 state2) sigma
  union (DFA s1 s d1 q1 f1) (DFA s2 _ d2 q2 f2) =
    DFA (product s1 s2)
        s
        (\(Tuple r1 r2) a -> Tuple (d1 r1 a) (d2 r2 a))
        (Tuple q1 q2)
        (product f1 s2 `S.union` product s1 f2)

  -- These should be in `purescript-sets` so they can be more efficient.
  isSubsetOf :: forall v. (Ord v) => S.Set v -> S.Set v -> Boolean
  isSubsetOf p q = all (flip S.member q) $ S.toList p

  product :: forall v1 v2. (Ord v1, Ord v2) => S.Set v1 -> S.Set v2 -> S.Set (Tuple v1 v2)
  product s1 s2 = S.fromList $ Tuple <$> S.toList s1 <*> S.toList s2
