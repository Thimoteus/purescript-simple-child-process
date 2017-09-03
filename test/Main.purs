module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.ChildProcess (CHILD_PROCESS)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects (fs :: FS, buffer :: BUFFER, cp :: CHILD_PROCESS)) Unit
main = discover "Node\\.ChildProcess\\.Simple.*Spec" >>= run [consoleReporter]
