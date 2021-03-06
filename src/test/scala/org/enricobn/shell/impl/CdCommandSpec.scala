package org.enricobn.shell.impl

import org.enricobn.shell.VirtualCommand
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/16/16.
  */
class CdCommandSpec extends FlatSpec with MockFactory with Matchers {
  private def fixture = {
    val fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, "root").right.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})

    implicit val authentication: Authentication = fs.vum.logRoot("root").right.get

    val _ = fs.root.mkdir("bin").right.get
    val home = fs.root.findFolder("home").right.get.get
    val _guest = home.mkdir("guest").right.get

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
    assert(result == Seq("/bin/", "/etc/", "/home/"), result)
  }

  "completion of 'cd /home/'" should "return /home/guest/ and /home/root" in {
    val f = fixture
    val result = f.command.completion("cd /home/", f.shell)
    assert(result == Seq("/home/guest/", "/home/root/"), result)
  }

  "completion of 'cd /h'" should "return /home/" in {
    val f = fixture
    val result = f.command.completion("cd /h", f.shell)
    assert(result == Seq("/home/"), result)
  }

  "completion of 'cd ../'" should "return ../guest/ and ../root" in {
    val f = fixture
    val result = f.command.completion("cd ../", f.shell)
    assert(result == Seq("../guest/", "../root/"), result)
  }

  "completion of 'cd ' in /home" should "return guest/ and root/" in {
    val f = fixture
    f.shell.currentFolder = f.shell.currentFolder.parent.get
    val result = f.command.completion("cd ", f.shell)
    assert(result == Seq("guest/", "root/"), result)
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
    assert(result == List("guest"), result)
  }

  "completion of 'cd /home/guest'" should "return one row" in {
    val f = fixture
    val result = f.command.completion("cd /home/guest", f.shell)
    assert(result == List("/home/guest"), result)
  }

  "completion of 'cd /home/guest/pippo'" should "return 2 rows" in {
    val f = fixture

    implicit val authentication : Authentication = f.shell.authentication

    val dummy = f.guestFolder.mkdir("dummy").right.get
    dummy.mkdir("indummy").right.get
    f.guestFolder.mkdir("dummy1").right.get

    val result = f.command.completion("cd /home/guest/dummy", f.shell)

    assert(result.toList == List("/home/guest/dummy/", "/home/guest/dummy1/"))
  }

  "completion of 'cd /home/user1'" should "return a row" in {
    val f = fixture

    implicit val authentication : Authentication = f.shell.authentication

    createUsersExample(f)

    val result = f.command.completion("cd /home/guest/user1", f.shell)

    assert(result.toList == List("/home/guest/user1"))
  }

  "completion of 'cd /home/user'" should "return 2 rows" in {
    val f = fixture

    implicit val authentication : Authentication = f.shell.authentication

    createUsersExample(f)

    val result = f.command.completion("cd /home/guest/user", f.shell)

    assert(result.toList == List("/home/guest/user1/", "/home/guest/user2/"))
  }

  "completion of 'cd user1' from home" should "return a row" in {
    val f = fixture

    implicit val authentication : Authentication = f.shell.authentication

    createUsersExample(f)

    f.shell.currentFolder = f.guestFolder

    val result = f.command.completion("cd user1", f.shell)

    assert(result.toList == List("user1"))
  }

  private def createUsersExample(f: Object {
    val shell: VirtualShellImpl

    val command: VirtualCommand

    val guestFolder: VirtualFolder
  }) = {
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
