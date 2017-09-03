-- | This module contains simplified functions to call child processes and
-- | return their results in Aff.
-- |
-- | It re-exports the CHILD_PROCESS effect.
-- |
-- | ```purescript
-- | import Node.ChildProcess.Simple (execAff')
-- |
-- | main = launchAff do
-- |   result <- execAff' "echo \"Hi\""
-- |   log result
-- | ```
module Node.ChildProcess.Simple
  ( execAff
  , execAff'
  , execFileAff
  , execFileAff'
  , module Exports
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Aff (Aff, makeAff)
import Data.Maybe (Maybe(Just, Nothing))
import Node.Buffer as Buffer
import Node.Buffer (BUFFER)
import Node.ChildProcess as CP
import Node.ChildProcess (CHILD_PROCESS) as Exports
import Node.Encoding (Encoding(UTF8))

-- | Type alias for the output of a child process.
type ProcessOutput = { stdout :: String, stderr :: String }

-- | Convenience function for makeAff.
_affCallback
  :: forall e
   . (Error -> Eff (buffer :: BUFFER | e) Unit)
  -> (ProcessOutput -> Eff (buffer :: BUFFER | e) Unit)
  -> (CP.ExecResult -> Eff (buffer :: BUFFER | e) Unit)
_affCallback reject accept =
  \({stdout, stderr, error}) ->
        case error of
          Just e -> reject e
          Nothing -> do
            out <- Buffer.toString UTF8 stdout
            err <- Buffer.toString UTF8 stderr
            accept {stdout: out, stderr: err}

-- | Run a command in the shell and return its stdout and stderr as Strings.
execAff
  :: forall e
   . String
  -> CP.ExecOptions
  -> Aff (buffer :: BUFFER, cp :: CP.CHILD_PROCESS | e) ProcessOutput
execAff cmd opts = makeAff $ \reject accept ->
  CP.exec cmd opts (_affCallback reject accept)

-- | Run a command in the shell using defaultExecOptions and return its stdout
-- | as a String.
execAff'
  :: forall e
   . String
  -> Aff (buffer :: BUFFER, cp :: CP.CHILD_PROCESS | e) String
execAff' cmd = map (_.stdout) $ execAff cmd CP.defaultExecOptions

-- | Run a command directly from a file and return its stdout and stderr as
-- | Strings.
execFileAff
  :: forall e
   . String
  -> Array String
  -> CP.ExecOptions
  -> Aff (buffer :: BUFFER, cp :: CP.CHILD_PROCESS | e) ProcessOutput
execFileAff cmd args opts = makeAff $ \reject accept ->
  CP.execFile cmd args opts (_affCallback reject accept)

-- | Run a command directly from a file using defaultExecOptionos and return its
-- | stdout as a String.
execFileAff'
  :: forall e
   . String
  -> Array String
  -> Aff (buffer :: BUFFER, cp :: CP.CHILD_PROCESS | e) String
execFileAff' cmd args = map (_.stdout) $
  execFileAff cmd args CP.defaultExecOptions
