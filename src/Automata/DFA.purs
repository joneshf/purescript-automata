module Automata.DFA
  ( DFA()
  , DFAError(..)
  , dfa
  , accepts
  ) where

  import Data.Foldable (all)
  import Data.Set (Set(), member, toList)
  import Data.Validation (V(), invalid)

  -- | A Deterministic Finite Automaton (DFA) is a 5-tuple (Q, Σ, δ, q₀, F)
  -- | We only expose the type.
  -- | We leave the smart constructor to validate a potential DFA.
  data DFA state sigma = DFA (Set state)               -- Q  The set of states.
                             (Set sigma)               -- Σ  The alphabet of transitions.
                             (state -> sigma -> state) -- δ  The transition function.
                             state                     -- q₀ The initial state.
                             (Set state)               -- F  The set of accepting states.

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
      go q []     = q `member` f
      go q (s:ss) = go (d q s) ss

  -- | Attempt to construct and validate a DFA.
  dfa :: forall sigma state
      .  (Ord sigma, Ord state)
      => Set state
      -> Set sigma
      -> (state -> sigma -> state)
      -> state
      -> Set state
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
    | q0 `member` states = pure dfa
    | otherwise          = invalid [StartState]

  validateAccept :: forall sigma state
                 .  (Ord sigma, Ord state)
                 => DFA state sigma
                 -> V [DFAError] (DFA state sigma)
  validateAccept dfa@(DFA states _ _ _ f)
    | f `isSubsetOf` states = pure dfa
    | otherwise             = invalid [AcceptStates]

  -- This should be in `purescript-sets` so it can be more efficient.
  isSubsetOf :: forall v. (Ord v) => Set v -> Set v -> Boolean
  isSubsetOf p q = all (flip member q) $ toList p
