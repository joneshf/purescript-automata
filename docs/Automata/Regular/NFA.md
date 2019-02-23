## Module Automata.Regular.NFA

#### `NFA`

``` purescript
data NFA state sigma
```

##### Instances
``` purescript
(Ord sigma, Ord state1, Ord state2) => Union (NFA state1 sigma) (NFA state2 sigma) (NFA (UnionStates state1 state2) sigma)
(Ord sigma, Ord state1, Ord state2) => Concatenate (NFA state1 sigma) (NFA state2 sigma) (NFA (ConcatenateStates state1 state2) sigma)
```

#### `NFAError`

``` purescript
data NFAError
```

##### Instances
``` purescript
Show NFAError
```

#### `ConcatenateStates`

``` purescript
data ConcatenateStates s1 s2
```

##### Instances
``` purescript
(Ord sigma, Ord state1, Ord state2) => Concatenate (NFA state1 sigma) (NFA state2 sigma) (NFA (ConcatenateStates state1 state2) sigma)
(Eq s1, Eq s2) => Eq (ConcatenateStates s1 s2)
(Ord s1, Ord s2) => Ord (ConcatenateStates s1 s2)
```

#### `UnionStates`

``` purescript
data UnionStates s1 s2
```

##### Instances
``` purescript
(Ord sigma, Ord state1, Ord state2) => Union (NFA state1 sigma) (NFA state2 sigma) (NFA (UnionStates state1 state2) sigma)
(Eq s1, Eq s2) => Eq (UnionStates s1 s2)
(Ord s1, Ord s2) => Ord (UnionStates s1 s2)
```

#### `accepts`

``` purescript
accepts :: forall sigma state. Ord sigma => Ord state => NFA state sigma -> List (Epsilon sigma) -> Boolean
```

#### `nfa`

``` purescript
nfa :: forall sigma state. Ord sigma => Ord state => Set state -> Set sigma -> (state -> Epsilon sigma -> Set state) -> state -> Set state -> V (List NFAError) (NFA state sigma)
```

Attempt to construct and validate an NFA.


