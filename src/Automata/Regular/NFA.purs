module Automata.Regular.NFA
  ( NFA()
  , NFAError(..)
  , accepts
  , nfa
  ) where

  import Automata.Epsilon (Epsilon(..))

  import Data.Foldable (all, foldr)
  import Data.Tuple (Tuple(..))
  import Data.Validation (V(), invalid)

  import Data.Set (Set(), difference, empty, fromList, insert, member, singleton, toList, union, unions)

  import qualified Automata.Combinators as C

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
      .  (Ord sigma, Ord state)
      => Set state
      -> Set sigma
      -> (state -> Epsilon sigma -> Set state)
      -> state
      -> Set state
      -> V [NFAError] (NFA state sigma)
  nfa states sigma d q0 f = validate (NFA states sigma d q0 f)

  validate :: forall sigma state
           .  (Ord sigma, Ord state)
           => NFA state sigma
           -> V [NFAError] (NFA state sigma)
  validate = pure

  accepts :: forall sigma state
          .  (Ord sigma, Ord state)
          => NFA state sigma
          -> [Epsilon sigma]
          -> Boolean
  accepts (NFA _ _ d q0 f) inputs = go (singleton q0) inputs
    where
      go qs []     = qs `intersects` f
      go qs (s:ss) = go (unions $ flip d s <$> toList qs) ss

  unionImpl :: forall sigma state1 state2
           .  (Ord sigma, Ord state1, Ord state2)
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
      delta U0     Epsilon = fromList [U1 q1, U2 q2]
      delta U0     _       = empty
      delta (U1 q) a       = go1 $ d1 q a
      delta (U2 q) a       = go2 $ d2 q a
      go1 q1 = fromList $ U1 <$> toList q1
      go2 q2 = fromList $ U2 <$> toList q2

  data UnionStates s1 s2 = U0 | U1 s1 | U2 s2

  instance eqUnionStates :: (Eq s1, Eq s2) => Eq (UnionStates s1 s2) where
    (==) U0     U0      = true
    (==) (U1 q) (U1 q') = q == q'
    (==) (U2 q) (U2 q') = q == q'
    (/=) q      q'      = not (q == q')

  instance ordUnionStates :: (Ord s1, Ord s2) => Ord (UnionStates s1 s2) where
    compare U0     _       = LT
    compare U0     U0      = EQ

    compare (U1 _) (U2 _)  = LT
    compare (U1 q) (U1 q') = compare q q'

    compare (U2 q) (U2 q') = compare q q'

    compare q      q'      = compare q' q

  concatenateImpl :: forall sigma state1 state2
                  .  (Ord sigma, Ord state1, Ord state2)
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
      go1 q1 = fromList $ C1 <$> toList q1
      go2 q2 = fromList $ C2 <$> toList q2

  data ConcatenateStates s1 s2 = C1 s1 | C2 s2

  instance eqConcatenateStates :: (Eq s1, Eq s2) => Eq (ConcatenateStates s1 s2) where
    (==) (C1 q) (C1 q') = q == q'
    (==) (C2 q) (C2 q') = q == q'
    (/=) q      q'      = not (q == q')

  instance ordConcatenateStates :: (Ord s1, Ord s2) => Ord (ConcatenateStates s1 s2) where
    compare (C1 _) (C2 _)  = LT
    compare (C1 q) (C1 q') = compare q q'

    compare (C2 q) (C2 q') = compare q q'

    compare q      q'      = compare q' q

  -- These should be in `purescript-sets` so they can be more efficient.
  isSubsetOf :: forall v. (Ord v) => Set v -> Set v -> Boolean
  isSubsetOf p q = all (flip member q) $ toList p

  product :: forall v1 v2. (Ord v1, Ord v2) => Set v1 -> Set v2 -> Set (Tuple v1 v2)
  product s1 s2 = fromList $ Tuple <$> toList s1 <*> toList s2

  intersects :: forall v. (Ord v) => Set v -> Set v -> Boolean
  intersects s1 s2 = s1 `difference` s2 /= s1
