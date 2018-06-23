package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs._
import org.enricobn.vfs.impl.UnixLikeInMemoryFS
import org.scalamock.matchers.ArgThat
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/15/16.
  */
class ShellCompletionsSpec extends FlatSpec with MockFactory with Matchers {
  private val fs = UnixLikeInMemoryFS("root").right.get

  private implicit val authentication: Authentication = fs.vum.logRoot("root").right.get

  def fixture = {
    new {
      val context: VirtualShellContext = stub[VirtualShellContext]
      val completions = new ShellCompletions(context)
      val shell = stub[VirtualShell]
    }
  }

  "empty line" should "return NoProposals" in {
    val f = fixture

    val result = f.completions.complete("", f.shell)
    result match {
      case NoProposals() =>
      case _ => fail(result.toString)
    }
  }

  "a partial command" should "return NewLine if there's only one command match" in {
    val f = fixture
    val cat = stubCommandFile(f.shell, "cat")
    val ls = stubCommandFile(f.shell, "ls")

    stubPath(f.context, Set(cat, ls))

    val result = f.completions.complete("c", f.shell)

    result match {
      case NewLine(line) => assert(line == "cat ")
      case _ => fail(result.toString)
    }
  }

  "no matching partial command" should "return NoProposals" in {
    val f = fixture
    val ls = stubCommandFile(f.shell, "ls")

    stubPath(f.context, Set(ls))

    val result = f.completions.complete("c", f.shell)
    result match {
      case NoProposals() =>
      case _ => fail(result.toString)
    }
  }

  "a partial command" should "return Proposals if there's more than one command match" in {
    val f = fixture
    val cat = stubCommandFile(f.shell, "cat")
    val ls = stubCommandFile(f.shell, "cd")

    stubPath(f.context, Set(cat, ls))

    val result = f.completions.complete("c", f.shell)
    result match {
      case Proposals(props) => assert(props == Seq("cat", "cd"))
      case _ => fail(result.toString)
    }
  }

  "a complete command with no arguments" should "return Proposals from the command itself" in {
    val f = fixture
    val catCommand = stub[VirtualCommand]

    (catCommand.completion _).when("cat ", *).returns(Seq("hello", "world"))
    val cat = stubCommandFile(f.shell, "cat", catCommand)

    stubPath(f.context, Set(cat))

    val result = f.completions.complete("cat ", f.shell)
    result match {
      case Proposals(props) => assert(props == Seq("hello", "world"))
      case _ => fail(result.toString)
    }
  }

  private def stubCommandFile(shell: VirtualShell, name: String, virtualCommand: VirtualCommand = stub[VirtualCommand]) = {
    val commandFile = stub[VirtualFile]
    (commandFile.name _).when().returns(name)

    val permission = stub[VirtualPermission]
    (permission.execute _).when().returns(true)

    (commandFile.getCurrentUserPermission(_ : Authentication)).when(*).returns(Right(permission))

    (shell.findCommand(_ : String, _ : VirtualFolder)).when(name, *).returns(Right(Some(commandFile)))
    (commandFile.getContent(_ : Authentication)).when(*).returns(Right(virtualCommand))

    commandFile
  }

  private def sameRef[T](ref: T) : ArgThat[T] = new ArgThat[T](v => v.asInstanceOf[AnyRef] eq ref.asInstanceOf[AnyRef])

  private def stubPath(context: VirtualShellContext, files: Set[VirtualFile]): Unit = {
    val bin = stub[VirtualFolder]
    (bin.files(_ : Authentication)).when(*).returns(Right(files))
    (context.path(_ : Authentication)).when(*).returns(Right(Seq(bin)))
  }

}
