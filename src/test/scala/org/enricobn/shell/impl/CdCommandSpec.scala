package org.enricobn.shell.impl

import org.enricobn.vfs.VirtualUsersManager
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by enrico on 12/16/16.
  */
class CdCommandSpec extends FlatSpec with MockFactory with Matchers {
  def fixture = {
    val _vum = stub[VirtualUsersManager]
    val fs = new InMemoryFS(_vum)
    val bin = fs.root.mkdir("bin")
    val home = fs.root.mkdir("home")
    val guest = home.mkdir("guest")

    new {
      val command = new CdCommand
      val vum = _vum
      val currentFolder = guest
    }
  }

  "completion of 'cd '" should "return empty Seq" in {
    val f = fixture
    val result = f.command.completion("cd ", f.currentFolder)
    assert(result.isEmpty)
  }

  "completion of 'cd /'" should "return the list of root folders" in {
    val f = fixture
    val result = f.command.completion("cd /", f.currentFolder)
    assert(result == Seq("/bin/", "/home/"))
  }

  "completion of 'cd /home/'" should "return /home/guest/" in {
    val f = fixture
    val result = f.command.completion("cd /home/", f.currentFolder)
    assert(result == Seq("/home/guest/"))
  }

  "completion of 'cd /h'" should "return /home/" in {
    val f = fixture
    val result = f.command.completion("cd /h", f.currentFolder)
    assert(result == Seq("/home/"))
  }

  "completion of 'cd ../'" should "return ../guest/" in {
    val f = fixture
    val result = f.command.completion("cd ../", f.currentFolder)
    assert(result == Seq("../guest/"))
  }

  "completion of 'cd ' in /home" should "return guest/" in {
    val f = fixture
    val result = f.command.completion("cd ", f.currentFolder.parent)
    assert(result == Seq("guest/"))
  }

}
