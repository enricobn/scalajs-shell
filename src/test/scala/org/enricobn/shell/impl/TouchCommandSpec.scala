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
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/16/16.
  */
class TouchCommandSpec extends AnyFlatSpec with MockFactory with Matchers {
  private def fixture = {
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
      val command: VirtualCommand = TouchCommand
      val shell = new VirtualShellImpl(fs, stub[Terminal], fs.vum, fs.vsm, context, fs.root,
        authentication)

      fs.vum.addUser("guest", "guest", "guest")
      shell.login("guest", "guest")

      VirtualCommandOperations.createCommandFiles(bin, TouchCommand)
    }
  }

  "completion of 'touch /'" should "return a list" in {
    val f = fixture

    val result = f.command.completion("mkdir /", f.shell)

    assert(result.toList == List(Completion("/bin/", "bin/"), Completion("/etc/", "etc/"), Completion("/home/", "home/")))
  }

  "completion of 'touch /home/dummy'" should "return an empty list" in {
    val f = fixture

    val result = f.command.completion("touch /home/dummy", f.shell)

    assert(result.isEmpty)
  }

  "touch /home/dummy" should "fail since guest user has no rights" in {
    val f = fixture

    val result = f.shell.run("touch", "/home/dummy")

    assert(result.isLeft)
  }

  "touch /home/guest/dummy" should "work" in {
    val f = fixture

    val result = f.shell.run("touch", "/home/guest/dummy")

    assert(result.isRight, result.toString)

    val folderE = VirtualPath.absolute("home", "guest", "dummy")
      .flatMap(_.toFile(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

  "touch ~/dummy" should "work" in {
    val f = fixture

    val result = f.shell.run("touch", "~/dummy")

    assert(result.isRight, result.toString)

    val folderE = VirtualPath.absolute("home", "guest", "dummy")
      .flatMap(_.toFile(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

  "touch dummy in /home/guest" should "work" in {
    val f = fixture

    val guestFolder = VirtualPath.absolute("home", "guest")
      .flatMap(_.toFolder(f.shell.fs)(f.shell.authentication)).toOption.get

    f.shell.currentFolder = guestFolder

    val result = f.shell.run("touch", "dummy")

    assert(result.isRight, result.toString)

    val folderE = VirtualPath.absolute("home", "guest", "dummy")
      .flatMap(_.toFile(f.shell.fs)(f.shell.authentication))

    assert(folderE.isRight)
  }

}
