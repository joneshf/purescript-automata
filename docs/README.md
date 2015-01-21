# Module Documentation

## Module Automata.DFA

### Types

     | A Deterministic Finite Automaton (DFA) is a 5-tuple (Q, Î£, Î´, qâ, F)
     | We only expose the type.
     | We leave the smart constructor to validate a potential DFA.

    data DFA state sigma

     F  The set of accepting states.
     | A DFA can be invalid in one of two conditions.
     | TODO: It'd be nice to validate that the transition function is total
     |       Maybe with `purescript-totally`

    data DFAError where
      StartState :: DFAError
      AcceptStates :: DFAError


### Type Class Instances

     F  â Q

    instance showDFAError :: Show DFAError


### Values

     | A DFA accepts a string in the alphabet
     | if the last transition puts the DFA in one of the accepting states.

    accepts :: forall sigma state. (Ord sigma, Ord state) => DFA state sigma -> [sigma] -> Boolean

     | Attempt to construct and validate a DFA.

    dfa :: forall sigma state. (Ord sigma, Ord state) => Set state -> Set sigma -> (state -> sigma -> state) -> state -> Set state -> V [DFAError] (DFA state sigma)



