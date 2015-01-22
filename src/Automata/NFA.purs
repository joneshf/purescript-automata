module Automata.NFA where

  import Data.Foldable (foldr)

  import qualified Data.Set as S

  data NFA state sigma = NFA (S.Set state)
                             (S.Set sigma)
                             (state -> Epsilon sigma -> S.Set state)
                             state
                             (S.Set state)

  data Epsilon sigma = Epsilon | Sigma sigma

  accepts :: forall sigma state
          .  (Ord sigma, Ord state)
          => NFA state sigma
          -> [Epsilon sigma]
          -> Boolean
  accepts (NFA _ _ d q0 f) inputs = go (S.singleton q0) inputs
    where
      go qs []     = qs `intersects` f
      go qs (s:ss) = go (S.unions $ flip d s <$> S.toList qs) ss

  intersects :: forall v. (Ord v) => S.Set v -> S.Set v -> Boolean
  intersects s1 s2 = s1 `S.difference` s2 /= s1
