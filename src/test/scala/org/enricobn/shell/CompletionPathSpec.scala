package org.enricobn.shell

import org.enricobn.shell.impl.{CatCommand, VirtualShellContextImpl, VirtualShellImpl}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.reflectiveCalls

class CompletionPathSpec extends AnyFlatSpec with MockFactory with Matchers {

  private def fixture = {
    val fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, "root").toOption.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})

    implicit val authentication: Authentication = fs.vum.logRoot("root").toOption.get

    val _ = fs.root.mkdir("bin").toOption.get
    val home = fs.root.findFolder("home").toOption.get.get
    val _guest = home.mkdir("guest").toOption.get

    new {
      val command: VirtualCommand = CatCommand
      val guestFolder: VirtualFolder = _guest
      val shell = new VirtualShellImpl(fs, stub[Terminal], fs.vum, fs.vsm, new VirtualShellContextImpl(), guestFolder,
        authentication)
    }
  }

  "completion for ../user1" should "work" in {
    val f = fixture

    implicit val authentication: Authentication = f.shell.authentication

    createUsersExample(f)

    val path = CompletionPath(f.shell, "../user1", forFile = true)

    path match {
      case PartialPath(_, relativePath, _) =>
        assert(relativePath == "../user1/")
      case _ => fail()
    }

  }

  private def createUsersExample(f: Object {
    val shell: VirtualShellImpl

    val command: VirtualCommand

    val guestFolder: VirtualFolder
  }): Unit = {
    implicit val authentication: Authentication = f.shell.authentication

    val errorOrUnit: Either[IOError, Unit] = for {
      usr1 <- f.guestFolder.mkdir("user1")
      city1 <- usr1.mkdir("city1")
      usr2 <- f.guestFolder.mkdir("user2")
      city2 <- usr2.mkdir("city2")
      _ <- city1.touch("file")
      _ <- city2.touch("file1")
    } yield {
      f.shell.currentFolder = usr1
    }

    errorOrUnit.left.foreach(e => fail(e.message))
  }
}
