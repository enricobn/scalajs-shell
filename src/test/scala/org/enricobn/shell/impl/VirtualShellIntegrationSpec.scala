package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.terminal.Terminal
import org.enricobn.vfs._
import org.scalamock.scalatest.MockFactory
import org.scalatest._

import scala.collection.mutable.ListBuffer

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/12/16.
  */
class VirtualShellIntegrationSpec extends FlatSpec with MockFactory with Matchers with OneInstancePerTest  {
  private val rootPassword = "root"
  private var terminal : Terminal = _
  private var scheduler :  FakeScheduler = _
  private var fs : UnixLikeInMemoryFS = _
  private var shell : VirtualShell = _
  private var textFile : VirtualFile = _
  private var rootAuthentication : Authentication = _
  private var binFile : VirtualFile = _
  private var rootFile : VirtualFile = _
  private var usrFile : VirtualFile = _

  import org.enricobn.vfs.utils.Utils.RightBiasedEither

  terminal = mock[Terminal]
  scheduler = new FakeScheduler()

  fs = UnixLikeInMemoryFS(rootPassword).right.get

  implicit val _rootAuthentication: Authentication = fs.vum.logRoot(rootPassword).right.get

  private val init = for {
    _ <- fs.vum.addUser("guest", "guest").toLeft(())
    _authentication <- fs.vum.logUser("guest", "guest")
    _rootFile <- fs.root.touch("rootFile")
    _usrFile <- fs.usr.touch("usrFile")
    guestHome <- fs.home.resolveFolderOrError("guest")
    text <- guestHome.touch("text.txt")
    _ <- text.chmod(666).toLeft(())
    _ <- text.setContent("Hello\nWorld").toLeft(())
    _binFile <- fs.usrBin.touch("binFile")
    _ <- VirtualCommandOperations.createCommandFile(fs.usrBin, new InteractiveCommand())

    virtualShell = UnixLikeVirtualShell(fs, terminal, guestHome, _authentication, scheduler)

    _ <- VirtualCommandOperations.createCommandFiles(fs.bin, new LsCommand(), new CatCommand(), new CdCommand())

    _ = (terminal.add _).expects(where { message: String => message.contains("/home/guest") })
    _ = (terminal.flush _).expects().anyNumberOfTimes()
    _ = (terminal.onInput _).expects(*).anyNumberOfTimes()
    _ = (terminal.removeOnInputs _).expects().anyNumberOfTimes()
    _ = (terminal.removeOnInput _).expects(*).anyNumberOfTimes()

    _ = virtualShell.start()
  } yield {
    shell = virtualShell
    textFile = text
    rootAuthentication = _rootAuthentication
    binFile = _binFile
    rootFile = _rootFile
    usrFile = _usrFile
  }

  init match {
    case Right(f) => f
    case Left(error) => fail(error.message)
  }

  "ls" should "show text.txt" in {
    (terminal.add _).expects(where {
      message: String => message.contains(".profile")
    })

    (terminal.add _).expects(where {
      message: String => message.contains("text.txt") && message.contains("rw- rw- rw-")
    })

    TestUtils.expectPrompt(terminal)

    shell.run("ls")
  }

  "ls background" should "show text.txt" in {
    (terminal.add _).expects(where {
      message: String => message.contains(".profile")
    })

    (terminal.add _).expects(where {
      message: String => message.contains("text.txt") && message.contains("rw- rw- rw-")
    })

    TestUtils.expectPrompt(terminal)

    shell.runInBackground("ls")
  }

  "cd" should "show bin, home and usr" in {
    (terminal.add _).expects(where {
      message: String => message.contains("bin") && message.contains("rwx rwx r-x")
    })

    (terminal.add _).expects(where {
      message: String => message.contains("etc") && message.contains("rwx rwx r-x")
    })

    (terminal.add _).expects(where {
      message: String => message.contains("home") && message.contains("rwx rwx r-x")
    })

    (terminal.add _).expects(where {
      message: String => message.contains("usr") && message.contains("rwx rwx r-x")
    })

    (terminal.add _).expects(where {
      message: String => message.contains("rootFile") && message.contains("rw- rw- r--")
    })

    (terminal.add _).expects(where {
      message: String => message.contains("var") && message.contains("rwx rwx r-x")
    })

    TestUtils.expectPrompt(terminal)
    shell.run("cd", "/")

    TestUtils.expectPrompt(terminal)
    shell.run("ls")

  }

  "cd to not existent folder" should "return an error" in {
    assertError(
      shell.run("cd", "foo"),
      "cd: folder: foo: no such directory"
    )
  }

  "running text.txt" should "return an error" in {
    textFile.setExecutable(rootAuthentication)

    assertError(
      shell.run("text.txt"),
      "File is not a command."
    )
  }

  "toFolder of root" should "be root" in {
    val folder = shell.toFolder("/")

    assert(folder.right.get == fs.root)
  }

  "toFolder of absolute path" should "work" in {
    val folder = shell.toFolder("/usr/bin")

    assert(folder.right.get == fs.usrBin)
  }

  "toFolder of relative path" should "work" in {
    shell.currentFolder = fs.usr

    val folder = shell.toFolder("bin")

    assert(folder.right.get == fs.usrBin)
  }

  "toFolder of parent path" should "work" in {
    shell.currentFolder = fs.bin

    val folder = shell.toFolder("../bin")

    assert(folder.right.get == fs.bin)
  }

  "toFolder of self" should "work" in {
    shell.currentFolder = fs.usr

    val folder = shell.toFolder("./bin")

    assert(folder.right.get == fs.usrBin)
  }

  "findFolder of not existent folder" should "return Right(None)" in {
    val folder = shell.findFolder("home/enrico")

    assert(folder.right.get.isEmpty)
  }

  "toFile of absolute path" should "work" in {
    val file = shell.toFile("/usr/bin/binFile")

    assert(binFile == file.right.get)
  }

  "toFile of root file" should "work" in {
    val file = shell.toFile("/rootFile")

    assert(rootFile == file.right.get)
  }

  "toFile of relative path" should "work" in {
    shell.currentFolder = fs.usrBin

    val file = shell.toFile("../usrFile")

    assert(usrFile == file.right.get)
  }

  "findFile of parent of root" should "return Right(None)" in {
    shell.currentFolder = fs.root

    val file = shell.findFile("..")

    assert(Right(None) == file)
  }

  "findFile of parent of parent of root" should "return Right(None)" in {
    shell.currentFolder = fs.root

    val file = shell.findFile("../..")

    assert(Right(None) == file)
  }

  "findFolder of ../.. of usr" should "return Right(None)" in {
    shell.currentFolder = fs.usr

    val file = shell.findFolder("../..")

    assert(Right(None) == file)
  }

  "findFolder of ../.. of bin" should "return root" in {
    shell.currentFolder = fs.usrBin

    val folder = shell.findFolder("../..")

    assert(Right(Some(fs.root)) == folder)
  }

  "findFile of ../../usr/bin/binFile from bin" should "return binFile" in {
    shell.currentFolder = fs.usrBin

    val file = shell.findFile("../../usr/bin/binFile")

    assert(Right(Some(binFile)) == file)
  }

  "find of ../..." should "not work, but don't throw an Exception" in {
    shell.currentFolder = fs.bin

    val file = shell.findFile("../.../")

    assert(Right(None) == file)
  }

  "find of ~/text.txt" should "work" in {
    val file = shell.findFile("~/text.txt")

    assert(Right(Some(textFile)) == file)
  }

  "int" should "show prompt" in {
    TestUtils.expectPrompt(terminal)
    shell.run("int")
    scheduler.join()
  }

  private class InteractiveCommand() extends VirtualCommand {
    override def name: String = "int"

    override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*): Either[IOError, VirtualProcess] = {
      var _running = true
      var count = 5

      Right(new VirtualProcess {
        override def update(): Unit = {
          super.update()
            count = count - 1
            if (count >= 0) {
              _running = false
            }
        }

        override def running: Boolean = _running
      })
    }

    override def completion(line: String, shell: VirtualShell): Seq[String] = Seq.empty
  }

  private class FakeScheduler extends Scheduler {
    private var count = 2

    private val callbacks = new ListBuffer[Double => Unit]()

    private val mainThread = new Thread(new Runnable {
      override def run(): Unit = {
        while (count >= 0) {
          while (callbacks.isEmpty) {
            Thread.sleep(100)
          }
          count = count - 1
          callbacks.remove(0).apply(0)
          Thread.sleep(100)
        }
      }
    })

    mainThread.start()

    override def run(callback: Double => Unit): Unit = {
      callbacks.append(callback)
    }

    def join(): Unit = {
      mainThread.join()
    }
  }

  private def assertError(result: Either[IOError, Unit], message: String): Unit = {
    result match {
      case Left(error) => assert(error.message == message)
      case _ => fail("Should return an error.")
    }
  }
}
