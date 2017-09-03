module Node.ChildProcess.SimpleSpec where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Node.Buffer (BUFFER)
import Node.ChildProcess (defaultExecOptions)
import Node.ChildProcess.Simple
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec (buffer :: BUFFER, cp :: CHILD_PROCESS | r) Unit
spec = do
  describe "execAff" do
    it "runs a command and returns its output as a String" do
      {stdout, stderr} <- execAff "cat test/example/example.txt" defaultExecOptions
      stdout `shouldEqual` "EXAMPLE\n"
  describe "execAff'" do
    it "runs a command with defaultExecOptions and returns its output as a String" do
      result <- execAff' "cat test/example/example.txt"
      result `shouldEqual` "EXAMPLE\n"
  describe "execFileAff" do
    it "runs a command and returns its output as a String" do
      {stdout, stderr} <- execFileAff "cat" ["test/example/example.txt"] defaultExecOptions
      stdout `shouldEqual` "EXAMPLE\n"
  describe "execFileAff'" do
    it "runs a command with defaultExecOptions and returns its output as a String" do
      result <- execFileAff' "cat" ["test/example/example.txt"]
      result `shouldEqual` "EXAMPLE\n"
