package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/12/16.
  */
class VirtualShellIntegrationSpec extends FlatSpec with MockFactory with Matchers {
  def fixture = {
    val term = mock[Terminal]

    val rootPassword = "root"
    val vum = new VirtualUsersManagerImpl(rootPassword)

    vum.addUser("guest", "guest")

    val fs = new InMemoryFS(vum)

    val _rootFile = fs.root.touch("rootFile").right.get
    val _bin = fs.root.mkdir("bin").right.get

    val _usr = fs.root.mkdir("usr").right.get
    val _usrFile = _usr.touch("usrFile").right.get
    val usrBin = _usr.mkdir("bin").right.get
    val _homeFolder = fs.root.mkdir("home").right.get
    val _guestFolder = _homeFolder.mkdir("guest").right.get
    val text = _guestFolder.touch("text.txt").right.get
    text.chmod(666)
    val _binFile = usrBin.touch("binFile").right.get

    val context = new VirtualShellContextImpl()
    context.createCommandFile(_bin, new LsCommand())
    context.createCommandFile(_bin, new CdCommand())
    context.createCommandFile(_bin, new CatCommand())
    context.addToPath(_bin)
    context.addToPath(usrBin)
    val virtualShell = new VirtualShell(term, vum, context, _guestFolder)

    vum.logUser("guest", "guest")
    text.content = "Hello\nWorld"

    (term.add _).expects(where {message: String => message.contains("/home/guest")})
    (term.flush _).expects().anyNumberOfTimes()
    (term.onInput _).expects(*).anyNumberOfTimes()

    virtualShell.start()

    new {
      val shell = virtualShell
      val terminal = term
      val textFile = text
      val virtualUsersManager = vum
      val root = fs.root
      val bin = usrBin
      val binFile = _binFile
      val usr = _usr
      val rootFile = _rootFile
      val usrFile = _usrFile
      val guest = _guestFolder
    }
  }

  "ls" should "show text.txt" in {
    val f = fixture

    (f.terminal.add _).expects(where {
      message: String => message.contains("text.txt") && message.contains("rw- rw- rw-")
    })

    (f.terminal.removeOnInputs _).expects().times.repeat(2)

    assertPrompt(
      f.shell.run("ls")
    )
  }

  "cd" should "show bin, home and usr" in {
    val f = fixture

    (f.terminal.add _).expects(where {
      message: String => message.contains("bin") && message.contains("rwx rwx r-x")
    })

    (f.terminal.add _).expects(where {
      message: String => message.contains("home") && message.contains("rwx rwx r-x")
    })

    (f.terminal.add _).expects(where {
      message: String => message.contains("usr") && message.contains("rwx rwx r-x")
    })

    (f.terminal.add _).expects(where {
      message: String => message.contains("rootFile") && message.contains("rw- rw- r--")
    })

    (f.terminal.removeOnInputs _).expects().times.repeat(2)
    assertPrompt(f.shell.run("cd", "/"))

    (f.terminal.removeOnInputs _).expects().times.repeat(2)

    assertPrompt(
      f.shell.run("ls")
    )
  }

  "cd to not existent folder" should "return an error" in {
    val f = fixture

    (f.terminal.removeOnInputs _).expects().times.repeat(2)

    assertError(
      f.shell.run("cd", "foo"),
      "cd: foo: No such file or directory"
    )
  }

  "running text.txt" should "return an error" in {
    val f = fixture

    f.virtualUsersManager.logUser("root", "root")

    f.textFile.setExecutable()

    (f.terminal.removeOnInputs _).expects().times.repeat(1)

    assertError(
      f.shell.run("text.txt"),
      "File is not a command."
    )
  }

  "toFolder of root" should "be root" in {
    val f = fixture

    val folder = f.shell.toFolder("/")

    assert(folder.right.get == f.root)
  }

  "toFolder of absolute path" should "work" in {
    val f = fixture

    val folder = f.shell.toFolder("/usr/bin")

    assert(folder.right.get == f.bin)
  }

  "toFolder of relative path" should "work" in {
    val f = fixture

    f.shell.currentFolder = f.usr

    val folder = f.shell.toFolder("bin")

    assert(folder.right.get == f.bin)
  }

  "toFolder of parent path" should "work" in {
    val f = fixture

    f.shell.currentFolder = f.bin

    val folder = f.shell.toFolder("../bin")

    assert(folder.right.get == f.bin)
  }

  "toFolder of self" should "work" in {
    val f = fixture

    f.shell.currentFolder = f.usr

    val folder = f.shell.toFolder("./bin")

    assert(folder.right.get == f.bin)
  }

  "findFolder of not existent folder" should "return Right(None)" in {
    val f = fixture

    val folder = f.shell.findFolder("home/enrico")

    assert(folder.right.get.isEmpty)
  }

  "toFile of absolute path" should "work" in {
    val f = fixture

    val file = f.shell.toFile("/usr/bin/binFile")

    assert(f.binFile == file.right.get)
  }

  "toFile of root file" should "work" in {
    val f = fixture

    val file = f.shell.toFile("/rootFile")

    assert(f.rootFile == file.right.get)
  }

  "toFile of relative path" should "work" in {
    val f = fixture

    f.shell.currentFolder = f.bin

    val file = f.shell.toFile("../usrFile")

    assert(f.usrFile == file.right.get)
  }

  "findFile of parent of root" should "return Right(None)" in {
    val f = fixture

    f.shell.currentFolder = f.root

    val file = f.shell.findFile("..")

    assert(Right(None) == file)
  }

  "findFile of parent of parent of root" should "return Right(None)" in {
    val f = fixture

    f.shell.currentFolder = f.root

    val file = f.shell.findFile("../..")

    assert(Right(None) == file)
  }

  "findFolder of ../.. of usr" should "return Right(None)" in {
    val f = fixture

    f.shell.currentFolder = f.usr

    val file = f.shell.findFolder("../..")

    assert(Right(None) == file)
  }

  "findFolder of ../.. of bin" should "return root" in {
    val f = fixture

    f.shell.currentFolder = f.bin

    val folder = f.shell.findFolder("../..")

    assert(Right(Some(f.root)) == folder)
  }

  "findFile of ../../usr/bin/binFile from bin" should "return binFile" in {
    val f = fixture

    f.shell.currentFolder = f.bin

    val file = f.shell.findFile("../../usr/bin/binFile")

    assert(Right(Some(f.binFile)) == file)
  }

  "find of ../..." should "not work, but don't throw an Exception" in {
    val f = fixture

    f.shell.currentFolder = f.bin

    val file = f.shell.findFile("../.../")

    assert(Right(None) == file)
  }

  "find of ~/text.txt" should "work" in {
    val f = fixture

    val file = f.shell.findFile("~/text.txt")

    assert(Right(Some(f.textFile)) == file)
  }

  private def assertPrompt(result: Either[IOError, Boolean], expectedPrompt : Boolean = true): Unit = {
    result match {
      case Left(error) => fail(error.message)
      case Right(prompt) => assert(prompt == expectedPrompt)
    }
  }

  private def assertError(result: Either[IOError, Boolean], message: String): Unit = {
    result match {
      case Left(error) => assert(error.message == message)
      case _ => fail("Should return an error.")
    }
  }
}
