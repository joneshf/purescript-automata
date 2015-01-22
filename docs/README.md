# Module Documentation

## Module Automata.Combinators

### Type Classes


    class Concatenate a b c where

      concatenate :: a -> b -> c


    class Union a b c where

      union :: a -> b -> c


## Module Automata.Epsilon

### Types


    data Epsilon sigma where
      Epsilon :: Epsilon sigma
      Sigma :: sigma -> Epsilon sigma


## Module Automata.Regular.DFA

### Types

     | A Deterministic Finite Automaton (DFA) is a 5-tuple (Q, Î£, Î´, qâ, F)
     | We only expose the type.
     | We leave the smart constructor to validate a potential DFA.

    data DFA state sigma

     | A DFA can be invalid in one of two conditions.
     | TODO: It'd be nice to validate that the transition function is total
     |       Maybe with `purescript-totally`

    data DFAError where
      StartState :: DFAError
      AcceptStates :: DFAError


### Type Class Instances

     F  â Q

    instance showDFAError :: Show DFAError

     F  The set of accepting states.

    instance unionDFA :: (Ord sigma, Ord state1, Ord state2) => C.Union (DFA state1 sigma) (DFA state2 sigma) (DFA (Tuple state1 state2) sigma)


### Values

     | A DFA accepts a string in the alphabet
     | if the last transition puts the DFA in one of the accepting states.

    accepts :: forall sigma state. (Ord sigma, Ord state) => DFA state sigma -> [sigma] -> Boolean

     | Attempt to construct and validate a DFA.

    dfa :: forall sigma state. (Ord sigma, Ord state) => Set state -> Set sigma -> (state -> sigma -> state) -> state -> Set state -> V [DFAError] (DFA state sigma)


## Module Automata.Regular.NFA

### Types


    data NFA state sigma


    data NFAError


### Type Class Instances


    instance eqUnionStates :: (Eq s1, Eq s2) => Eq (UnionStates s1 s2)


    instance ordUnionStates :: (Ord s1, Ord s2) => Ord (UnionStates s1 s2)


    instance showNFAError :: Show NFAError


    instance unionNFA :: (Ord sigma, Ord state1, Ord state2) => C.Union (NFA state1 sigma) (NFA state2 sigma) (NFA (UnionStates state1 state2) sigma)


### Values


    accepts :: forall sigma state. (Ord sigma, Ord state) => NFA state sigma -> [Epsilon sigma] -> Boolean

     | Attempt to construct and validate an NFA.

    nfa :: forall sigma state. (Ord sigma, Ord state) => Set state -> Set sigma -> (state -> Epsilon sigma -> Set state) -> state -> Set state -> V [NFAError] (NFA state sigma)



