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
import Test.Unit.Main as Test.Unit.Main

main :: Effect Unit
main = Test.Unit.Main.runTest do
  Test.Automata.Regular.DFA.OddOnes.suite
  Test.Automata.Regular.DFA.Turnstile.suite
  Test.Automata.Regular.DFA.ZeroZeroOne.suite
  Test.Automata.Regular.NFA.A.suite
  Test.Automata.Regular.NFA.AB.suite
  Test.Automata.Regular.NFA.B.suite
  Test.Automata.Regular.NFA.OneThirdFromEnd.suite
