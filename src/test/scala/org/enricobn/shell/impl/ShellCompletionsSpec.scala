package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.{VirtualFile, VirtualFolder, VirtualPermission}
import org.scalamock.matchers.ArgThat
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/15/16.
  */
class ShellCompletionsSpec extends FlatSpec with MockFactory with Matchers {
  def fixture = {
    new {
      val context = stub[VirtualShellContext]
      val completions = new ShellCompletions(context)
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
    val cat = stubCommandFile(f.context, "cat")
    val ls = stubCommandFile(f.context,"ls")

    stubPath(f.context, Set(cat, ls))

    val result = f.completions.complete("c", currentFolder)

    result match {
      case NewLine(line) => assert(line == "cat ")
      case _ => fail(result.toString)
    }
  }

  "no matching partial command" should "return NoProposals" in {
    val f = fixture
    val currentFolder = stub[VirtualFolder]
    val ls = stubCommandFile(f.context, "ls")

    stubPath(f.context, Set(ls))

    val result = f.completions.complete("c", currentFolder)
    result match {
      case NoProposals() =>
      case _ => fail(result.toString)
    }
  }

  "a partial command" should "return Proposals if there's more than one command match" in {
    val f = fixture
    val currentFolder = stub[VirtualFolder]
    val cat = stubCommandFile(f.context, "cat")
    val ls = stubCommandFile(f.context, "cd")

    stubPath(f.context, Set(cat, ls))

    val result = f.completions.complete("c", currentFolder)
    result match {
      case Proposals(props) => assert(props == Seq("cat", "cd"))
      case _ => fail(result.toString)
    }
  }

  "a complete command with no arguments" should "return Proposals from the command itself" in {
    val f = fixture
    val currentFolder = stub[VirtualFolder]
    val catCommand = stub[VirtualCommand]
    (catCommand.completion _).when("cat ", sameRef(currentFolder)).returns(Seq("hello", "world"))
    val cat = stubCommandFile(f.context, "cat", catCommand)

    stubPath(f.context, Set(cat))

    val result = f.completions.complete("cat ", currentFolder)
    result match {
      case Proposals(props) => assert(props == Seq("hello", "world"))
      case _ => fail(result.toString)
    }
  }

  private def stubCommandFile(context: VirtualShellContext, name: String, virtualCommand: VirtualCommand = stub[VirtualCommand]) : VirtualFile = {
    val commandFile = stub[VirtualFile]
    (commandFile.name _).when().returns(name)

    val permission = stub[VirtualPermission]
    (permission.execute _).when().returns(true)

    (commandFile.getCurrentUserPermission _).when().returns(permission)

    (context.findCommand _).when(name, *).returns(Some(commandFile))
    (context.getCommand _).when(sameRef(commandFile)).returns(Right(virtualCommand))

    commandFile
  }

  private def sameRef[T](ref: T) : ArgThat[T] = new ArgThat[T](v => v.asInstanceOf[AnyRef] eq ref.asInstanceOf[AnyRef])

  private def stubPath(context: VirtualShellContext, files: Set[VirtualFile]): Unit = {
    val bin = stub[VirtualFolder]
    (bin.files _).when().returns(Right(files))
    (context.path _).when().returns(Seq(bin))
  }

}
