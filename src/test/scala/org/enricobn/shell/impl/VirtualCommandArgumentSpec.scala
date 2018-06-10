package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.{VirtualFile, VirtualFolder, VirtualSecurityManager, VirtualUsersManager}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls

class VirtualCommandArgumentSpec extends FlatSpec with MockFactory with Matchers {

  private def fixture = {
    val _vsm = stub[VirtualSecurityManager]
    val _vum = stub[VirtualUsersManager]
    (_vsm.checkWriteAccess _).when(*).returns(true)
    (_vsm.checkReadAccess _).when(*).returns(true)
    (_vsm.checkExecuteAccess _).when(*).returns(true)


    val f = new {
      val usersManager: VirtualUsersManager = _vum
      val fs = new InMemoryFS(_vum, _vsm)
      val usr : VirtualFolder = fs.root.mkdir("usr").right.get
      val bin : VirtualFolder = usr.mkdir("bin").right.get
      val rootFile : VirtualFile = fs.root.touch("rootFile").right.get
      val usrFile : VirtualFile = usr.touch("usrFile").right.get
      val binFile : VirtualFile = bin.touch("binFile").right.get
      val shell = new VirtualShell(stub[Terminal], _vum, _vsm, new VirtualShellContextImpl(), bin)
    }
    (f.usersManager.currentUser _).when().returns("foo")
    f
  }

  "completion of FileArgument" should "be fine" in {
    val f = fixture

    val sut = FileArgument("file", true)
    val completions = sut.complete(f.shell, "/", Seq.empty)

    assert(completions == List("/rootFile", "/usr/"))
  }

  "parse of FileArgument" should "be fine" in {
    val f = fixture

    val sut = FileArgument("file", true)
    val parsedArguments = sut.parse(f.shell, "/rootFile", Seq.empty)

    assert(parsedArguments.right.get == f.rootFile)
  }

  "parse of FolderArgument" should "be fine" in {
    val f = fixture

    val sut = FolderArgument("folder", true)
    val parsedArguments = sut.parse(f.shell, "/", Seq.empty)

    assert(parsedArguments.right.get == f.fs.root)
  }

  "completion of not existent argument" should "return an empty list" in {
    val f = fixture

    val sut = new VirtualCommandArguments(FileArgument("file", true))

    val completions = sut.complete(f.shell, "command / ")

    assert(completions == Seq.empty)
  }

  "parse without required arguments" should "return an Error with usage" in {
    val f = fixture

    val sut = new VirtualCommandArguments(FileArgument("file", true))

    val parsedArguments = sut.parse(f.shell, "cd")

    assert(parsedArguments.isLeft)

    assert(parsedArguments.left.get == "usage: cd file ")
  }

  "parse without optional arguments" should "be fine" in {
    val f = fixture

    val sut = new VirtualCommandArguments(FileArgument("file", false))

    val parsedArguments = sut.parse(f.shell, "cd")

    assert(parsedArguments.right.get.isEmpty)
  }

  "parse with optional arguments before required" should "throw an exception" in {
    intercept[IllegalArgumentException] {
      new VirtualCommandArguments(FileArgument("file1", false), FileArgument("file2", true))
    }
  }

  "parse with required arguments before optional" should "be fine" in {
    new VirtualCommandArguments(FileArgument("file1", true), FileArgument("file2", false))
  }

  "complete with no arguments" should "be empty" in {
    val sut = new VirtualCommandArguments()

    val f = fixture

    assert(sut.complete(f.shell, "cd").isEmpty)
  }

  "parse with no arguments" should "be empty" in {
    val sut = new VirtualCommandArguments()

    val f = fixture

    assert(sut.parse(f.shell, "cd").equals(Right(Seq.empty)))
  }
}
