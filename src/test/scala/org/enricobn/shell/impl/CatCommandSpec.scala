package org.enricobn.shell.impl

import org.enricobn.shell.{Completion, VirtualCommand}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.{Authentication, VirtualFolder}
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

// to access members of structural types (new {}) without warnings
import scala.reflect.Selectable.reflectiveSelectable

/**
  * Created by enrico on 12/16/16.
  */
class CatCommandSpec extends AnyFlatSpec with MockFactory with Matchers {

  private def fixture: Object {val command: VirtualCommand; val guestFolder: VirtualFolder; val shell: VirtualShellImpl} = {
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

  "completion of 'cat f'" should "return one element" in {
    val f = fixture

    implicit val authentication: Authentication = f.shell.authentication

    f.guestFolder.touch("file").toOption.get

    val result = f.command.completion("cat f", f.shell)

    assert(result == List(Completion("file", "file")))
  }

  "completion of 'cat f'" should "return two element" in {
    val f = fixture

    implicit val authentication: Authentication = f.shell.authentication

    f.guestFolder.touch("file").toOption.get
    f.guestFolder.touch("file1").toOption.get

    val result = f.command.completion("cat f", f.shell)

    assert(result == List(Completion("file", "file"), Completion("file1", "file1")))
  }

}
