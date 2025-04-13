package org.enricobn.shell.impl

import org.enricobn.shell.Completion
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.*
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.reflectiveCalls

class VirtualCommandArgumentSpec extends AnyFlatSpec with MockFactory with Matchers {

  private def fixture = {
    val rootPassword = "rootPassword"
    val _fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, rootPassword).toOption.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})
    implicit val authentication: Authentication = _fs.vum.logRoot(rootPassword).toOption.get

    new {
      val fs: InMemoryFS = _fs
      val usersManager: VirtualUsersManager = _fs.vum
      val usr : VirtualFolder = _fs.root.mkdir("usr").toOption.get
      val bin : VirtualFolder = usr.mkdir("bin").toOption.get
      val rootFile : VirtualFile = _fs.root.touch("rootFile").toOption.get
      val usrFile : VirtualFile = usr.touch("usrFile").toOption.get
      val binFile : VirtualFile = bin.touch("binFile").toOption.get
      val shell = new VirtualShellImpl(fs, stub[Terminal], _fs.vum, _fs.vsm, new VirtualShellContextImpl(), bin, authentication)
    }
  }

  "completion of FileArgument" should "be fine" in {
    val f = fixture

    val sut = FileArgument("file", required = true)
    val completions = sut.complete(f.shell, "/", Seq.empty)

    assert(completions == List(Completion("/rootFile", "rootFile"), Completion("/etc/", "etc/"),
      Completion("/home/", "home/"), Completion("/usr/", "usr/")))
  }

  "parse of FileArgument" should "be fine" in {
    val f = fixture

    val sut = FileArgument("file", required = true)
    val parsedArguments = sut.parse(f.shell, "/rootFile", Seq.empty)

    assert(parsedArguments.toOption.get == f.rootFile)
  }

  "parse of FolderArgument" should "be fine" in {
    val f = fixture

    val sut = FolderArgument("folder", required = true)
    val parsedArguments = sut.parse(f.shell, "/", Seq.empty)

    assert(parsedArguments.toOption.get == f.fs.root)
  }

  "completion of not existent argument" should "return an empty list" in {
    val f = fixture

    val sut = new VirtualCommandArguments(FileArgument("file", required = true))

    val completions = sut.complete(f.shell, "command / ")

    assert(completions == Seq.empty)
  }

  "parse without required arguments" should "return an Error with usage" in {
    val f = fixture

    val sut = new VirtualCommandArguments(FileArgument("file", required = true))

    val parsedArguments = sut.parse(f.shell, "cd")

    assert(parsedArguments.isLeft)

    assert(parsedArguments == Left("usage: cd file "))
  }

  "parse without optional arguments" should "be fine" in {
    val f = fixture

    val sut = new VirtualCommandArguments(FileArgument("file", required = false))

    val parsedArguments = sut.parse(f.shell, "cd")

    assert(parsedArguments.toOption.get.isEmpty)
  }

  "parse with optional arguments before required" should "throw an exception" in {
    intercept[IllegalArgumentException] {
      new VirtualCommandArguments(FileArgument("file1", required = false), FileArgument("file2", required = true))
    }
  }

  "parse with required arguments before optional" should "be fine" in {
    new VirtualCommandArguments(FileArgument("file1", required = true), FileArgument("file2", required = false))
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
