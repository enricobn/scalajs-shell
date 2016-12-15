package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.{VirtualFile, VirtualFolder, VirtualPermission, VirtualPermissions}
import org.scalamock.matchers.ArgThat
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/15/16.
  */
class ShellCompletionsSpec extends FlatSpec with MockFactory with Matchers {
  def fixture = {
    new {
      val path = new ShellPathImpl
      val completions = new ShellCompletions(path)
    }
  }

  "empty line" should "return NoProposals" in {
    val f = fixture
    val currentFolder = stub[VirtualFolder]

    val result = f.completions.complete("", currentFolder)
    result match {
      case NoProposals() =>
      case _ => fail(result.toString)
    }
  }

  "a partial command" should "return NewLine if there's only one command match" in {
    val f = fixture
    val currentFolder = stub[VirtualFolder]
    val cat = stubCommandFile("cat")
    val ls = stubCommandFile("ls")

    val bin = stub[VirtualFolder]

    (bin.files _).when().returns(Set(cat, ls))

    f.path.add(bin)

    val result = f.completions.complete("c", currentFolder)

    result match {
      case NewLine(line) => assert(line == "cat ")
      case _ => fail(result.toString)
    }
  }

  "no matching partial command" should "return NoProposals" in {
    val f = fixture
    val currentFolder = stub[VirtualFolder]
    val ls = stubCommandFile("ls")

    val bin = stub[VirtualFolder]

    (bin.files _).when().returns(Set(ls))

    f.path.add(bin)

    val result = f.completions.complete("c", currentFolder)
    result match {
      case NoProposals() =>
      case _ => fail(result.toString)
    }
  }

  "a partial command" should "return Proposals if there's more than one command match" in {
    val f = fixture
    val currentFolder = stub[VirtualFolder]
    val cat = stubCommandFile("cat")
    val ls = stubCommandFile("cd")

    val bin = stub[VirtualFolder]

    (bin.files _).when().returns(Set(cat, ls))

    f.path.add(bin)

    val result = f.completions.complete("c", currentFolder)
    result match {
      case Proposals(props) => assert(props == Seq("cat", "cd"))
      case _ => fail(result.toString)
    }
  }

  "a complete command with no arguments" should "return Proposals from the command itself" in {
    val f = fixture
    val currentFolder = stub[VirtualFolder]
    val cat = stubCommandFile("cat")

    (cat.path _).when().returns("/bin/cat")

    val catCommand = stub[VirtualCommand]
    (catCommand.completion _).when("cat ", sameRef(currentFolder)).returns(Seq("hello", "world"))

    f.completions.addCommandFile(cat, catCommand)

    val bin = stub[VirtualFolder]
    (bin.findFile(_: String)).when("cat").returns(Some(cat))

    (bin.files _).when().returns(Set(cat))

    f.path.add(bin)

    val result = f.completions.complete("cat ", currentFolder)
    result match {
      case Proposals(props) => assert(props == Seq("hello", "world"))
      case _ => fail(result.toString)
    }
  }

  def stubCommandFile(name: String) : VirtualFile = {
    val command = stub[VirtualFile]
    (command.name _).when().returns(name)

    val permission = stub[VirtualPermission]
    (permission.execute _).when().returns(true)

    (command.getCurrentUserPermission _).when().returns(permission)
    command
  }

  def sameRef[T](ref: T) : ArgThat[T] = new ArgThat[T](v => v.asInstanceOf[AnyRef] eq ref.asInstanceOf[AnyRef])

}
