package org.enricobn.shell.impl

import org.enricobn.shell.{VirtualCommand, VirtualCommandOperations, VirtualShellProfile}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.{Authentication, IOError, VirtualPath}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/16/16.
  */
class MkdirCommandSpec extends FlatSpec with MockFactory with Matchers {
  private def fixture = {
    val fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, "root").right.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})

    implicit val authentication: Authentication = fs.vum.logRoot("root").right.get

    val bin = fs.root.mkdir("bin").right.get

    val context = new VirtualShellContextImpl()
    context.setGlobalProfile(new VirtualShellProfile() {
      override def add(key: String, value: String): Either[IOError, Unit] = Right(())

      override def keys: Either[IOError, Set[String]] = Right(Set("PATH"))

      override def apply(key: String): Either[IOError, Option[String]] = Right(Some("/bin"))
    })

    new {
      val command: VirtualCommand = MkdirCommand
      val shell = new VirtualShellImpl(fs, stub[Terminal], fs.vum, fs.vsm, context, fs.root,
        authentication)

      fs.vum.addUser("guest", "guest", "guest")
      shell.login("guest", "guest")

      VirtualCommandOperations.createCommandFiles(bin, MkdirCommand)
    }
  }

  "completion of 'mkdir /'" should "return a list" in {
    val f = fixture

    val result = f.command.completion("mkdir /", f.shell)

    assert(result.toList == List("/bin/", "/etc/", "/home/"))
  }

  "completion of 'mkdir /home/dummy'" should "return an empty list" in {
    val f = fixture

    val result = f.command.completion("mkdir /home/dummy", f.shell)

    assert(result.isEmpty)
  }

  "mkdir /home/dummy" should "fail since guest user has no rights" in {
    val f = fixture

    val result = f.shell.run("mkdir", "/home/dummy")

    assert(result.isLeft)
  }

  "mkdir /home/guest/dummy" should "work" in {
    val f = fixture

    val result = f.shell.run("mkdir", "/home/guest/dummy")

    assert(result.isRight)

    val folderE = VirtualPath.absolute("home", "guest", "dummy").right
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

  "mkdir ~/dummy" should "work" in {
    val f = fixture

    val result = f.shell.run("mkdir", "~/dummy")

    assert(result.isRight)

    val folderE = VirtualPath.absolute("home", "guest", "dummy").right
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

  "mkdir dummy in /home/guest" should "work" in {
    val f = fixture

    val guestFolder = VirtualPath.absolute("home", "guest").right
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication)).right.get

    f.shell.currentFolder = guestFolder

    val result = f.shell.run("mkdir", "dummy")

    assert(result.isRight)

    val folderE = VirtualPath.absolute("home", "guest", "dummy").right
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

}
