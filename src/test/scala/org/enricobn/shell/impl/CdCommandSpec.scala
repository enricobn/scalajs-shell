package org.enricobn.shell.impl

import org.enricobn.shell.VirtualCommand
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
class CdCommandSpec extends FlatSpec with MockFactory with Matchers {
  private def fixture = {
    val fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, "root").right.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})

    implicit val authentication: Authentication = fs.vum.logRoot("root").right.get

    val bin = fs.root.mkdir("bin").right.get
    val home = fs.root.findFolder("home").right.get.get
    val guest = home.mkdir("guest").right.get

    new {
      val command: VirtualCommand = CdCommand
      val currentFolder: VirtualFolder = guest
      val shell = new VirtualShellImpl(fs, stub[Terminal], fs.vum, fs.vsm, new VirtualShellContextImpl(), currentFolder,
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

  "completion of 'cd guest' from /home" should "return no completions" in {
    val f = fixture
    f.shell.currentFolder = f.shell.currentFolder.parent.get
    val result = f.command.completion("cd guest", f.shell)
    assert(result.isEmpty, result)
  }

  "completion of 'cd /home/guest'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd /home/guest", f.shell)
    assert(result.isEmpty, result)
  }

  "completion of 'cd /home/guest/'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd /home/guest/", f.shell)
    assert(result.isEmpty, result)
  }
}
