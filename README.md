# purescript-simple-child-process

Simplified wrappers for common [Node.ChildProcess][Node.ChildProcess] operations.

## Installation

```sh
bower install purescript-simple-child-process
```

## Usage

```purescript
module Main where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Buffer (BUFFER)

import Node.ChildProcess.Simple (CHILD_PROCESS, execAff')

type ExecEffects e = (buffer :: BUFFER, cp :: CHILD_PROCESS, exception :: EXCEPTION | e)

main :: forall e. Eff (ExecEffects (console :: CONSOLE | e)) Unit
main = void $ launchAff do
  result <- execAff' "echo \"Hi\""
  log result
```

## Documentation

Module documentation is [published on Pursuit][Pursuit].

[Node.ChildProcess]: https://github.com/purescript-node/purescript-node-child-process
[Pursuit]: http://pursuit.purescript.org/packages/purescript-simple-child-process
