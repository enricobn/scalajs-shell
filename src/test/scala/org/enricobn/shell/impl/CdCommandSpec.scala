package org.enricobn.shell.impl

import org.enricobn.shell.{Completion, VirtualCommand}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

// to access members of structural types (new {}) without warnings
import scala.reflect.Selectable.reflectiveSelectable

/**
  * Created by enrico on 12/16/16.
  */
class CdCommandSpec extends AnyFlatSpec with MockFactory with Matchers {
  private def fixture: Object {val command: VirtualCommand; val guestFolder: VirtualFolder; val shell: VirtualShellImpl} = {
    val fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, "root").toOption.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})

    implicit val authentication: Authentication = fs.vum.logRoot("root").toOption.get

    val _ = fs.root.mkdir("bin").toOption.get
    val home = fs.root.findFolder("home").toOption.get.get
    val _guest = home.mkdir("guest").toOption.get

    new {
      val command: VirtualCommand = CdCommand
      val guestFolder: VirtualFolder = _guest
      val shell = new VirtualShellImpl(fs, stub[Terminal], fs.vum, fs.vsm, new VirtualShellContextImpl(), guestFolder,
        authentication)
    }
  }

  "completion of 'cd '" should "return empty Seq" in {
    val f = fixture
    val result = f.command.completion("cd ", f.shell)
    assert(result.isEmpty, result)
  }

  "completion of 'cd /'" should "return the list of root folders" in {
    val f = fixture
    val result = f.command.completion("cd /", f.shell)
    assert(result == Seq(Completion("/bin/", "bin"), Completion("/etc/", "etc"), Completion("/home/", "home")), result)
  }

  "completion of 'cd /home/'" should "return /home/guest/ and /home/root" in {
    val f = fixture
    val result = f.command.completion("cd /home/", f.shell)
    assert(result == Seq(Completion("/home/guest/", "guest"), Completion("/home/root/", "root")), result)
  }

  "completion of 'cd /h'" should "return /home/" in {
    val f = fixture
    val result = f.command.completion("cd /h", f.shell)
    assert(result == Seq(Completion("/home/", "home")), result)
  }

  "completion of 'cd ../'" should "return ../guest/ and ../root" in {
    val f = fixture
    val result = f.command.completion("cd ../", f.shell)
    assert(result == Seq(Completion("../guest/", "guest"), Completion("../root/", "root")), result)
  }

  "completion of 'cd ' in /home" should "return guest/ and root/" in {
    val f = fixture
    f.shell.currentFolder = f.shell.currentFolder.parent.get
    val result = f.command.completion("cd ", f.shell)
    assert(result == Seq(Completion("guest/", "guest"), Completion("root/", "root")), result)
  }

  "completion of 'cd nonexistentpath'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd nonexistentpath", f.shell)
    assert(result.isEmpty, result)
  }

  "completion of 'cd ../nonexistentpath'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd ../nonexistentpath", f.shell)
    assert(result.isEmpty, result)
  }

  "completion of 'cd ../../../nonexistentpath'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd ../../../nonexistentpath", f.shell)
    assert(result.isEmpty, result)
  }

  "completion of 'cd guest' from /home" should "return one row" in {
    val f = fixture
    f.shell.currentFolder = f.shell.currentFolder.parent.get
    val result = f.command.completion("cd guest", f.shell)
    assert(result == List(Completion("guest", "guest")), result)
  }

  "completion of 'cd /home/guest'" should "return one row" in {
    val f = fixture
    val result = f.command.completion("cd /home/guest", f.shell)
    assert(result == List(Completion("/home/guest", "guest")), result)
  }

  "completion of 'cd /home/guest/pippo'" should "return 2 rows" in {
    val f = fixture

    implicit val authentication : Authentication = f.shell.authentication

    val dummy = f.guestFolder.mkdir("dummy").toOption.get
    dummy.mkdir("indummy").toOption.get
    f.guestFolder.mkdir("dummy1").toOption.get

    val result = f.command.completion("cd /home/guest/dummy", f.shell)

    assert(result.toList == List(Completion("/home/guest/dummy/", "dummy"), Completion("/home/guest/dummy1/", "dummy1")))
  }

  "completion of 'cd /home/user1'" should "return a row" in {
    val f = fixture

    implicit val authentication : Authentication = f.shell.authentication

    createUsersExample(f)

    val result = f.command.completion("cd /home/guest/user1", f.shell)

    assert(result.toList == List(Completion("/home/guest/user1", "user1")))
  }

  "completion of 'cd /home/user'" should "return 2 rows" in {
    val f = fixture

    implicit val authentication : Authentication = f.shell.authentication

    createUsersExample(f)

    val result = f.command.completion("cd /home/guest/user", f.shell)

    assert(result.toList == List(Completion("/home/guest/user1/", "user1"), Completion("/home/guest/user2/", "user2")))
  }

  "completion of 'cd user1' from home" should "return a row" in {
    val f = fixture

    implicit val authentication : Authentication = f.shell.authentication

    createUsersExample(f)

    f.shell.currentFolder = f.guestFolder

    val result = f.command.completion("cd user1", f.shell)

    assert(result.toList == List(Completion("user1", "user1")))
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
