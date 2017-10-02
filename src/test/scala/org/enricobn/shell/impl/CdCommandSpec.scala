package org.enricobn.shell.impl

import org.enricobn.vfs.VirtualUsersManager
import org.enricobn.vfs.inmemory.{InMemoryFS, InMemoryFolder}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/16/16.
  */
class CdCommandSpec extends FlatSpec with MockFactory with Matchers {
  private def fixture = {
    val _vum = stub[VirtualUsersManager]
    (_vum.checkWriteAccess _).when(*).returns(true)
    (_vum.checkReadAccess _).when(*).returns(true)
    (_vum.checkExecuteAccess _).when(*).returns(true)

    val fs = new InMemoryFS(_vum)
    val bin = fs.root.mkdir("bin").right.get
    val home = fs.root.mkdir("home").right.get
    val guest = home.mkdir("guest").right.get

    new {
      val command = new CdCommand
      val vum: VirtualUsersManager = _vum
      val currentFolder: InMemoryFolder = guest
    }
  }

  "completion of 'cd '" should "return empty Seq" in {
    val f = fixture
    val result = f.command.completion("cd ", f.currentFolder)
    assert(result.isEmpty, result)
  }

  "completion of 'cd /'" should "return the list of root folders" in {
    val f = fixture
    val result = f.command.completion("cd /", f.currentFolder)
    assert(result == Seq("/bin/", "/home/"), result)
  }

  "completion of 'cd /home/'" should "return /home/guest/" in {
    val f = fixture
    val result = f.command.completion("cd /home/", f.currentFolder)
    assert(result == Seq("/home/guest/"), result)
  }

  "completion of 'cd /h'" should "return /home/" in {
    val f = fixture
    val result = f.command.completion("cd /h", f.currentFolder)
    assert(result == Seq("/home/"), result)
  }

  "completion of 'cd ../'" should "return ../guest/" in {
    val f = fixture
    val result = f.command.completion("cd ../", f.currentFolder)
    assert(result == Seq("../guest/"), result)
  }

  "completion of 'cd ' in /home" should "return guest/" in {
    val f = fixture
    val result = f.command.completion("cd ", f.currentFolder.parent.get)
    assert(result == Seq("guest/"), result)
  }

  "completion of 'cd nonexistentpath'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd nonexistentpath", f.currentFolder)
    assert(result.isEmpty, result)
  }

  "completion of 'cd ../nonexistentpath'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd ../nonexistentpath", f.currentFolder)
    assert(result.isEmpty, result)
  }

  "completion of 'cd ../../../nonexistentpath'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd ../../../nonexistentpath", f.currentFolder)
    assert(result.isEmpty, result)
  }

  "completion of 'cd guest' from /home" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd guest", f.currentFolder.parent.get)
    assert(result.isEmpty, result)
  }

  "completion of 'cd /home/guest'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd /home/guest", f.currentFolder)
    assert(result.isEmpty, result)
  }

  "completion of 'cd /home/guest/'" should "return no completions" in {
    val f = fixture
    val result = f.command.completion("cd /home/guest/", f.currentFolder)
    assert(result.isEmpty, result)
  }
}
