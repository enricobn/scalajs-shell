package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs._
import org.enricobn.vfs.impl.UnixLikeInMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/12/16.
  */
class VirtualShellIntegrationSpec extends FlatSpec with MockFactory with Matchers {

  import org.enricobn.vfs.utils.Utils.RightBiasedEither

  def fixture = {
    val term = mock[Terminal]

    val rootPassword = "root"

    val _fs = UnixLikeInMemoryFS(rootPassword).right.get
    implicit val _rootAuthentication: Authentication = _fs.vum.logRoot(rootPassword).right.get

    val init = for {
      _ <- _fs.vum.addUser("guest", "guest").toLeft(())
      _authentication <- _fs.vum.logUser("guest", "guest")
      _rootFile <- _fs.root.touch("rootFile")
      _usrFile <- _fs.usr.touch("usrFile")
      guestHome <- _fs.home.resolveFolderOrError("guest")
      text <- guestHome.touch("text.txt")
      _ <- text.chmod(666).toLeft(())
      _ <- text.setContent("Hello\nWorld").toLeft(())
      _binFile <- _fs.usrBin.touch("binFile")

      context = new VirtualShellContextImpl(_fs)
      virtualShell = new VirtualShell(term, _fs.vum, _fs.vsm, context, guestHome, _authentication)
      _ = context.setProfile(new VirtualShellFileProfile(virtualShell))

      _ <- context.createCommandFiles(_fs.bin, new LsCommand(), new CatCommand(), new CdCommand())

      _ <- context.addToPath(_fs.bin)
      _ <- context.addToPath(_fs.usrBin)

      _ = (term.add _).expects(where {message: String => message.contains("/home/guest")})
      _ = (term.flush _).expects().anyNumberOfTimes()
      _ = (term.onInput _).expects(*).anyNumberOfTimes()

      _ = virtualShell.start()

    } yield new {
      val fs = _fs
      val shell = virtualShell
      val terminal = term
      val textFile = text
      val binFile = _binFile
      val rootFile = _rootFile
      val usrFile = _usrFile
      val guest = guestHome
      val rootAuthentication = _rootAuthentication
    }

    init match {
      case Right(f) => f
      case Left(error) => fail(error.message)
    }

  }

  "ls" should "show text.txt" in {
    val f = fixture

    (f.terminal.add _).expects(where {
      message: String => message.contains(".profile")
    })

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
      message: String => message.contains("etc") && message.contains("rwx rwx r-x")
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

    (f.terminal.add _).expects(where {
      message: String => message.contains("var") && message.contains("rwx rwx r-x")
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
      "cd: folder: foo: no such directory"
    )
  }

  "running text.txt" should "return an error" in {
    val f = fixture

    f.textFile.setExecutable(f.rootAuthentication)

    (f.terminal.removeOnInputs _).expects().times.repeat(1)

    assertError(
      f.shell.run("text.txt"),
      "File is not a command."
    )
  }

  "toFolder of root" should "be root" in {
    val f = fixture

    val folder = f.shell.toFolder("/")

    assert(folder.right.get == f.fs.root)
  }

  "toFolder of absolute path" should "work" in {
    val f = fixture

    val folder = f.shell.toFolder("/usr/bin")

    assert(folder.right.get == f.fs.usrBin)
  }

  "toFolder of relative path" should "work" in {
    val f = fixture

    f.shell.currentFolder = f.fs.usr

    val folder = f.shell.toFolder("bin")

    assert(folder.right.get == f.fs.usrBin)
  }

  "toFolder of parent path" should "work" in {
    val f = fixture

    f.shell.currentFolder = f.fs.bin

    val folder = f.shell.toFolder("../bin")

    assert(folder.right.get == f.fs.bin)
  }

  "toFolder of self" should "work" in {
    val f = fixture

    f.shell.currentFolder = f.fs.usr

    val folder = f.shell.toFolder("./bin")

    assert(folder.right.get == f.fs.usrBin)
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

    f.shell.currentFolder = f.fs.usrBin

    val file = f.shell.toFile("../usrFile")

    assert(f.usrFile == file.right.get)
  }

  "findFile of parent of root" should "return Right(None)" in {
    val f = fixture

    f.shell.currentFolder = f.fs.root

    val file = f.shell.findFile("..")

    assert(Right(None) == file)
  }

  "findFile of parent of parent of root" should "return Right(None)" in {
    val f = fixture

    f.shell.currentFolder = f.fs.root

    val file = f.shell.findFile("../..")

    assert(Right(None) == file)
  }

  "findFolder of ../.. of usr" should "return Right(None)" in {
    val f = fixture

    f.shell.currentFolder = f.fs.usr

    val file = f.shell.findFolder("../..")

    assert(Right(None) == file)
  }

  "findFolder of ../.. of bin" should "return root" in {
    val f = fixture

    f.shell.currentFolder = f.fs.usrBin

    val folder = f.shell.findFolder("../..")

    assert(Right(Some(f.fs.root)) == folder)
  }

  "findFile of ../../usr/bin/binFile from bin" should "return binFile" in {
    val f = fixture

    f.shell.currentFolder = f.fs.usrBin

    val file = f.shell.findFile("../../usr/bin/binFile")

    assert(Right(Some(f.binFile)) == file)
  }

  "find of ../..." should "not work, but don't throw an Exception" in {
    val f = fixture

    f.shell.currentFolder = f.fs.bin

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
