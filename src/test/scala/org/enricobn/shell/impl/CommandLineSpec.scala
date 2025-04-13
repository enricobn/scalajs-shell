package org.enricobn.shell.impl

import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Created by enrico on 12/21/16.
  */
class CommandLineSpec extends AnyFlatSpec with MockFactory with Matchers {

  "command without space" should "be an incomplete command" in {
    val sut = new CommandLine("cd")
    require(sut.commandName == "cd")
    assert(sut.incompleteCommand)
  }

  "command with space" should "NOT be an incomplete command" in {
    val sut = new CommandLine("cd ")
    require(sut.commandName == "cd")
    assert(!sut.incompleteCommand)
  }

  "command with argument without space" should "be an incomplete argument" in {
    val sut = new CommandLine("cd hello")
    require(sut.commandName == "cd")
    require(!sut.incompleteCommand)
    require(sut.args.length == 1)
    require(sut.lastArgument.contains("hello"))
    assert(sut.incompleteArgument)
  }

  "command with argument with space" should "NOT be an incomplete argument" in {
    val sut = new CommandLine("cd hello ")
    require(sut.commandName == "cd")
    require(!sut.incompleteCommand)
    require(sut.args.length == 1)
    require(sut.lastArgument.contains("hello"))
    assert(!sut.incompleteArgument)
  }

  "command with incomplete argument" should "be reconstructed correctly" in {
    val sut = new CommandLine("cd hel")
    assert(sut.reconstructLine("hello") == "cd hello")
  }

  "command with more arguments and incomplete argument" should "be reconstructed correctly" in {
    val sut = new CommandLine("cd hello wor")
    assert(sut.reconstructLine("world") == "cd hello world")
  }

  "command with argument and complete argument" should "be reconstructed correctly" in {
    val sut = new CommandLine("cd hello ")
    assert(sut.reconstructLine("world") == "cd hello world")
  }

  "command with argument with more than one space between command and argument" should "return one argument" in {
    val sut = new CommandLine("cd  hello")
    require(sut.commandName == "cd")
    assert(sut.args.length == 1)
    require(sut.lastArgument.contains("hello"))
  }

}
