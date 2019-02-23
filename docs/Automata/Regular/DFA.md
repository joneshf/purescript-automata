## Module Automata.Regular.DFA

#### `DFA`

``` purescript
data DFA state sigma
```

A Deterministic Finite Automaton (DFA) is a 5-tuple (Q, Σ, δ, q₀, F)
We only expose the type.
We leave the smart constructor to validate a potential DFA.

##### Instances
``` purescript
(Ord sigma, Ord state1, Ord state2) => Union (DFA state1 sigma) (DFA state2 sigma) (DFA (Tuple state1 state2) sigma)
```

#### `DFAError`

``` purescript
data DFAError
  = StartState
  | AcceptStates
```

A DFA can be invalid in one of two conditions.
TODO: It'd be nice to validate that the transition function is total
      Maybe with `purescript-totally`

##### Instances
``` purescript
Show DFAError
```

#### `accepts`

``` purescript
accepts :: forall sigma state. Ord sigma => Ord state => DFA state sigma -> List sigma -> Boolean
```

A DFA accepts a string in the alphabet
if the last transition puts the DFA in one of the accepting states.

#### `dfa`

``` purescript
dfa :: forall sigma state. Ord sigma => Ord state => Set state -> Set sigma -> (state -> sigma -> state) -> state -> Set state -> V (List DFAError) (DFA state sigma)
```

Attempt to construct and validate a DFA.


