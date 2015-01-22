module Automata.Combinators where

  class Union a b c where
    union :: a -> b -> c

  class Concatenate a b c where
    concatenate :: a -> b -> c
