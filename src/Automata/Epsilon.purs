module Automata.Epsilon where

  data Epsilon sigma = Epsilon | Sigma sigma

  instance eqEpsilon :: (Eq sigma) => Eq (Epsilon sigma) where
    (==) Epsilon   Epsilon    = true
    (==) (Sigma s) (Sigma s') = s == s'

    (/=) e         e'         = not (e == e')
