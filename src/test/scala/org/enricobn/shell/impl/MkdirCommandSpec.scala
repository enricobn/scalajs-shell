package org.enricobn.shell.impl

import org.enricobn.shell.{Completion, VirtualCommand, VirtualCommandOperations, VirtualShellProfile}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.{Authentication, IOError, VirtualPath}
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

// to access members of structural types (new {}) without warnings
import scala.reflect.Selectable.reflectiveSelectable

/**
  * Created by enrico on 12/16/16.
  */
class MkdirCommandSpec extends AnyFlatSpec with MockFactory with Matchers {
  private def fixture: Object {val command: VirtualCommand; val shell: VirtualShellImpl} = {
    val fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, "root").toOption.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})

    implicit val authentication: Authentication = fs.vum.logRoot("root").toOption.get

    val bin = fs.root.mkdir("bin").toOption.get

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

    assert(result.toList == List(Completion("/bin/", "bin"), Completion("/etc/", "etc"), Completion("/home/", "home")))
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

    val folderE = VirtualPath.absolute("home", "guest", "dummy")
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

  "mkdir ~/dummy" should "work" in {
    val f = fixture

    val result = f.shell.run("mkdir", "~/dummy")

    assert(result.isRight)

    val folderE = VirtualPath.absolute("home", "guest", "dummy")
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

  "mkdir dummy in /home/guest" should "work" in {
    val f = fixture

    val guestFolder = VirtualPath.absolute("home", "guest")
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication)).toOption.get

    f.shell.currentFolder = guestFolder

    val result = f.shell.run("mkdir", "dummy")

    assert(result.isRight)

    val folderE = VirtualPath.absolute("home", "guest", "dummy")
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

}
