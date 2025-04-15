package org.enricobn.shell.impl

import org.enricobn.shell.*
import org.enricobn.vfs.*
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

// to access members of structural types (new {}) without warnings
import scala.reflect.Selectable.reflectiveSelectable

/**
  * Created by enrico on 12/15/16.
  */
class ShellCompletionsSpec extends AnyFlatSpec with MockFactory with Matchers {
  private val _fs = InMemoryFS(
    {VirtualUsersManagerFileImpl(_, "root").toOption.get},
    {(_, vum) => new VirtualSecurityManagerImpl(vum)})
  private val fs = UnixLikeInMemoryFS(_fs, "root").toOption.get

  private implicit val authentication: Authentication = fs.vum.logRoot("root").toOption.get

  def fixture: Object {val context: VirtualShellContext; val completions: ShellCompletions; val shell: VirtualShell} = {
    new {
      val context: VirtualShellContext = stub[VirtualShellContext]
      val completions = new ShellCompletions(context)
      val shell: VirtualShell = stub[VirtualShell]
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

    stubPath(f.context, Seq(cat, ls))

    val result = f.completions.complete("c", f.shell)

    result match {
      case NewLine(line) => assert(line == "cat ")
      case _ => fail(result.toString)
    }
  }

  "no matching partial command" should "return NoProposals" in {
    val f = fixture
    val ls = stubCommandFile(f.shell, "ls")

    stubPath(f.context, Seq(ls))

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

    stubPath(f.context, Seq(cat, ls))

    val result = f.completions.complete("c", f.shell)
    result match {
      case Proposals(props) => assert(props == Seq(Completion("cat", "cat"), Completion("cd", "cd")))
      case _ => fail(result.toString)
    }
  }

  "a complete command with no arguments" should "return Proposals from the command itself" in {
    val f = fixture
    val catCommand = stub[VirtualCommand]

    catCommand.completion.when("cat ", *).returns(Seq(Completion("hello", "hello"), Completion("world", "world")))
    val cat = stubCommandFile(f.shell, "cat", catCommand)

    stubPath(f.context, Seq(cat))

    val result = f.completions.complete("cat ", f.shell)
    result match {
      case Proposals(props) => assert(props == Seq(Completion("hello", "hello"), Completion("world", "world")))
      case _ => fail(result.toString)
    }
  }

  private def stubCommandFile(shell: VirtualShell, name: String, virtualCommand: VirtualCommand = stub[VirtualCommand]) = {
    val commandFile = stub[VirtualFile]
    (() => commandFile.name).when().returns(name)
    (() => commandFile.parent).when().returns(Some(fs.usrBin))

    val permission = stub[VirtualPermission]
    (() => permission.execute).when().returns(true)

    (commandFile.getCurrentUserPermission(_ : Authentication)).when(*).returns(Right(permission))

    (shell.findCommand(_ : String, _ : VirtualFolder)).when(name, *).returns(Right(Some(commandFile)))
    (commandFile.getContent(_ : Authentication)).when(*).returns(Right(virtualCommand))

    commandFile
  }

  //private def sameRef[T](ref: T) : ArgThat[T] = new ArgThat[T](v => v.asInstanceOf[AnyRef] eq ref.asInstanceOf[AnyRef])

  private def stubPath(context: VirtualShellContext, files: Seq[VirtualFile]): Unit = {
    val bin = stub[VirtualFolder]
    (bin.files(_ : Authentication)).when(*).returns(Right(files))
    (context.path(_ : VirtualFS)(_ : Authentication)).when(*, *).returns(Right(Seq(bin)))
  }

}
