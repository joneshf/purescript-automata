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

  import Data.Set (Set(), difference, empty, fromList, member, singleton, toList, union, unions)

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
    NFA (singleton Q0 `union` s1' `union` s2')
        s
        delta
        Q0
        (f1' `union` f2')
    where
      delta Q0     Epsilon = fromList [Q1 q1, Q2 q2]
      delta Q0     _       = empty
      delta (Q1 q) a       = fromList $ Q1 <$> toList (d1 q a)
      delta (Q2 q) a       = fromList $ Q2 <$> toList (d2 q a)
      s1' = fromList $ Q1 <$> toList s1
      s2' = fromList $ Q2 <$> toList s2
      f1' = fromList $ Q1 <$> toList f1
      f2' = fromList $ Q2 <$> toList f2

  data UnionStates s1 s2 = Q0 | Q1 s1 | Q2 s2

  instance eqUnionStates :: (Eq s1, Eq s2) => Eq (UnionStates s1 s2) where
    (==) Q0     Q0      = true
    (==) (Q1 q) (Q1 q') = q == q'
    (==) (Q2 q) (Q2 q') = q == q'
    (/=) q      q'      = not (q == q')

  instance ordUnionStates :: (Ord s1, Ord s2) => Ord (UnionStates s1 s2) where
    compare Q0     _       = LT
    compare Q0     Q0      = EQ

    compare (Q1 _) (Q2 _)  = LT
    compare (Q1 q) (Q1 q') = compare q q'

    compare (Q2 q) (Q2 q') = compare q q'

    compare q      q'      = compare q' q


  -- These should be in `purescript-sets` so they can be more efficient.
  isSubsetOf :: forall v. (Ord v) => Set v -> Set v -> Boolean
  isSubsetOf p q = all (flip member q) $ toList p

  product :: forall v1 v2. (Ord v1, Ord v2) => Set v1 -> Set v2 -> Set (Tuple v1 v2)
  product s1 s2 = fromList $ Tuple <$> toList s1 <*> toList s2

  intersects :: forall v. (Ord v) => Set v -> Set v -> Boolean
  intersects s1 s2 = s1 `difference` s2 /= s1
