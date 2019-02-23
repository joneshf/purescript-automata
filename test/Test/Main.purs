module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Automata.Regular.DFA.OddOnes as Test.Automata.Regular.DFA.OddOnes
import Test.Automata.Regular.DFA.Turnstile as Test.Automata.Regular.DFA.Turnstile
import Test.Automata.Regular.DFA.ZeroZeroOne as Test.Automata.Regular.DFA.ZeroZeroOne
import Test.Automata.Regular.NFA.A as Test.Automata.Regular.NFA.A
import Test.Automata.Regular.NFA.AB as Test.Automata.Regular.NFA.AB
import Test.Automata.Regular.NFA.B as Test.Automata.Regular.NFA.B
import Test.Automata.Regular.NFA.OneThirdFromEnd as Test.Automata.Regular.NFA.OneThirdFromEnd

main :: Effect Unit
main = do
  Test.Automata.Regular.DFA.OddOnes.main
  Test.Automata.Regular.DFA.Turnstile.main
  Test.Automata.Regular.DFA.ZeroZeroOne.main
  Test.Automata.Regular.NFA.A.main
  Test.Automata.Regular.NFA.AB.main
  Test.Automata.Regular.NFA.B.main
  Test.Automata.Regular.NFA.OneThirdFromEnd.main
