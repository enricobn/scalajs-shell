package org.enricobn.shell.impl

import org.enricobn.shell.{Completion, VirtualCommand}
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.{Authentication, VirtualFolder}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/16/16.
  */
class CatCommandSpec extends FlatSpec with MockFactory with Matchers {

  private def fixture = {
    val fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, "root").right.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})

    implicit val authentication: Authentication = fs.vum.logRoot("root").right.get

    val _ = fs.root.mkdir("bin").right.get
    val home = fs.root.findFolder("home").right.get.get
    val _guest = home.mkdir("guest").right.get

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

    f.guestFolder.touch("file").right.get

    val result = f.command.completion("cat f", f.shell)

    assert(result == List(Completion("file", "file")))
  }

  "completion of 'cat f'" should "return two element" in {
    val f = fixture

    implicit val authentication: Authentication = f.shell.authentication

    f.guestFolder.touch("file").right.get
    f.guestFolder.touch("file1").right.get

    val result = f.command.completion("cat f", f.shell)

    assert(result == List(Completion("file", "file"), Completion("file1", "file1")))
  }

}
