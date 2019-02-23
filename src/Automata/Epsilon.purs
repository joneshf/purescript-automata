module Automata.Epsilon where

  import Prelude

  data Epsilon sigma = Epsilon | Sigma sigma

  instance eqEpsilon :: (Eq sigma) => Eq (Epsilon sigma) where
    eq Epsilon   Epsilon    = true
    eq (Sigma s) (Sigma s') = s == s'
    eq _ _ = false
