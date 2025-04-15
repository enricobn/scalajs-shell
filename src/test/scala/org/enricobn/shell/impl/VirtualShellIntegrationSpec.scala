package org.enricobn.shell.impl

import org.enricobn.shell.*
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.*
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.compiletime.uninitialized

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/12/16.
  */
class VirtualShellIntegrationSpec extends AnyFlatSpec with MockFactory with Matchers {
  private val rootPassword = "root"
  private var terminal: Terminal = uninitialized
  private var scheduler: FakeScheduler = uninitialized
  private var fs: UnixLikeInMemoryFS = uninitialized
  private var shell: VirtualShell = uninitialized
  private var textFile: VirtualFile = uninitialized
  private var rootAuthentication: Authentication = uninitialized
  private var binFile: VirtualFile = uninitialized
  private var rootFile: VirtualFile = uninitialized
  private var usrFile: VirtualFile = uninitialized
  private var context: VirtualShellContext = uninitialized
  private var guestHome: VirtualFolder = uninitialized

  terminal = mock[Terminal]
  scheduler = new FakeScheduler()
  context = mock[VirtualShellContext]

  val _fs: InMemoryFS = InMemoryFS(
    {
      VirtualUsersManagerFileImpl(_, rootPassword).toOption.get
    },
    { (_, vum) => new VirtualSecurityManagerImpl(vum) })

  fs = UnixLikeInMemoryFS(_fs, rootPassword).toOption.get

  implicit val _rootAuthentication: Authentication = fs.vum.logRoot(rootPassword).toOption.get

  private val init = for {
    _ <- fs.vum.addUser("guest", "guest", "guest")
    _authentication <- fs.vum.logUser("guest", "guest")
    _rootFile <- fs.root.touch("rootFile")
    _usrFile <- fs.usr.touch("usrFile")
    guestPath <- VirtualPath.absolute("home", "guest")
    _guestHome <- guestPath.toFolder(fs)
    text <- _guestHome.touch("text.txt")
    _ <- text.chmod(666)
    _ <- text.setContent("Hello\nWorld")
    _binFile <- fs.usrBin.touch("binFile")
    _ <- VirtualCommandOperations.createCommandFile(fs.usrBin, new InteractiveCommand())

    virtualShell = UnixLikeVirtualShell(fs, terminal, _guestHome, _authentication, scheduler)

    _ <- VirtualCommandOperations.createCommandFiles(fs.bin, LsCommand, CatCommand, CdCommand)

    _ = terminal.add.expects(where { (message: String) => message.contains("/home/guest") })
    _ = (() => terminal.flush()).expects().anyNumberOfTimes()
    _ = terminal.onInput.expects(*).anyNumberOfTimes()
    _ = (() => terminal.removeOnInputs()).expects().anyNumberOfTimes()
    _ = terminal.removeOnInput.expects(*).anyNumberOfTimes()

    _ = virtualShell.start()
  } yield {
    shell = virtualShell
    textFile = text
    rootAuthentication = _rootAuthentication
    binFile = _binFile
    rootFile = _rootFile
    usrFile = _usrFile
    guestHome = _guestHome
  }

  init match {
    case Right(f) => f
    case Left(error) => fail(error.message)
  }

  override def withFixture(test: NoArgTest): Outcome = { // Define a shared fixture
    // Shared setup (run at beginning of each test)
    scheduler.init()
    try test()
    finally {
      // Shared cleanup (run at end of each test)
    }
  }

  "ls" should "show text.txt" in {
    terminal.add.expects(where {
      (message: String) => message.contains(".profile")
    })

    terminal.add.expects(where {
      (message: String) => message.contains("text.txt") && message.contains("rw- rw- rw-")
    })

    expectPrompt(terminal)

    shell.run("ls")
  }

  /*
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

   */

  "cd" should "show bin, home and usr" in {
    terminal.add.expects(where {
      (message: String) => message.contains("bin") && message.contains("rwx rwx r-x")
    })

    terminal.add.expects(where {
      (message: String) => message.contains("etc") && message.contains("rwx rwx r-x")
    })

    terminal.add.expects(where {
      (message: String) => message.contains("home") && message.contains("rwx rwx r-x")
    })

    terminal.add.expects(where {
      (message: String) => message.contains("usr") && message.contains("rwx rwx r-x")
    })

    terminal.add.expects(where {
      (message: String) => message.contains("rootFile") && message.contains("rw- rw- r--")
    })

    terminal.add.expects(where {
      (message: String) => message.contains("var") && message.contains("rwx rwx r-x")
    })

    expectPrompt(terminal)
    shell.run("cd", "/")

    expectPrompt(terminal)
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

    expectPrompt(terminal)

    shell.run("cd", "/home/guest")

    assertError(
      shell.run("text.txt"),
      "File is not a command."
    )
  }

  "toFolder of root" should "be root" in {
    val folder = shell.toFolder("/")

    assert(folder.toOption.get == fs.root)
  }

  "toFolder of absolute path" should "work" in {
    val folder = shell.toFolder("/usr/bin")

    assert(folder.toOption.get == fs.usrBin)
  }

  "toFolder of relative path" should "work" in {
    shell.currentFolder = fs.usr

    val folder = shell.toFolder("bin")

    assert(folder.toOption.get == fs.usrBin)
  }

  "toFolder of parent path" should "work" in {
    shell.currentFolder = fs.bin

    val folder = shell.toFolder("../bin")

    assert(folder.toOption.get == fs.bin)
  }

  "toFolder of self" should "work" in {
    shell.currentFolder = fs.usr

    val folder = shell.toFolder("./bin")

    assert(folder.toOption.get == fs.usrBin)
  }

  "toFolder of not existent folder" should "return IOError" in {
    val folder = shell.toFolder("home/enrico")

    assert(folder.isLeft)
  }

  "toFile of absolute path" should "work" in {
    val file = shell.toFile("/usr/bin/binFile")

    assert(binFile == file.toOption.get)
  }

  "toFile of root file" should "work" in {
    val file = shell.toFile("/rootFile")

    assert(rootFile == file.toOption.get)
  }

  "toFile of relative path" should "work" in {
    shell.currentFolder = fs.usrBin

    val file = shell.toFile("../usrFile")

    assert(usrFile == file.toOption.get)
  }

  "findFile of parent of root" should "return IOError" in {
    shell.currentFolder = fs.root

    val file = shell.toFile("..")

    assert(file.isLeft)
  }

  "findFile of parent of parent of root" should "return IOError" in {
    shell.currentFolder = fs.root

    val file = shell.toFile("../..")

    assert(file.isLeft)
  }

  "findFolder of ../.. of usr" should "return IOError" in {
    shell.currentFolder = fs.usr

    val file = shell.toFolder("../..")

    assert(file.isLeft)
  }

  "findFolder of ../.. of bin" should "return root" in {
    shell.currentFolder = fs.usrBin

    val folder = shell.toFolder("../..")

    assert(Right(fs.root) == folder)
  }

  "findFile of ../../usr/bin/binFile from bin" should "return binFile" in {
    shell.currentFolder = fs.usrBin

    val file = shell.toFile("../../usr/bin/binFile")

    assert(Right(binFile) == file)
  }

  "find of ../..." should "not work, but don't throw an Exception" in {
    shell.currentFolder = fs.bin

    val file = shell.toFile("../.../")

    assert(file.isLeft)
  }

  "find of ~/text.txt" should "work" in {
    val file = shell.toFile("~/text.txt")

    assert(Right(textFile) == file)
  }

  "int" should "show prompt" in {
    expectPrompt(terminal)
    shell.run("int")
  }

  "minimumCommon" should "work" in {
    val result = VirtualShellImpl.minimumCommon(Seq("user1", "user2"))

    assert(result == "user")
  }

  "minimumCommon with one value" should "work" in {
    val result = VirtualShellImpl.minimumCommon(Seq("user1"))

    assert(result == "user1")
  }

  "minimumCommon with unrelated values" should "return an empty string" in {
    val result = VirtualShellImpl.minimumCommon(Seq("user1", "dummy"))

    assert(result == "")
  }

  "minimumCommon with empty seq" should "return an empty string" in {
    val result = VirtualShellImpl.minimumCommon(Seq.empty)

    assert(result == "")
  }

  "completion of i" should "return 'int '" in {
    expectPrompt(terminal)

    /*
    Here as an example on how to do it...
    (context.path(_ : VirtualFS)(_ : Authentication)).expects(where { (_: VirtualFS, _: Authentication) => true })
      .returns(Right(Seq(guestHome)))
     */

    terminalExpectCodePoints(27, 91, 48, 68)
    terminalExpectCodePoints(27, 91, 75)
    terminalExpectCodePoints(105)
    terminalExpectCodePoints(27, 91, 48, 68)
    terminalExpectCodePoints(27, 91, 49, 68)
    terminalExpectCodePoints(27, 91, 75)
    terminal.add.expects(where { (message: String) =>  message == "int " })

    (terminal.flush : () => Unit).expects().onCall(() => println("flush")).anyNumberOfTimes()

    shell.run("cd", "/home/guest")

    shell.asInstanceOf[VirtualShellImpl].inputHandler.apply("i")
    shell.asInstanceOf[VirtualShellImpl].inputHandler.apply(Terminal.TAB)
  }

  def terminalExpectCodePoints(codePoints: Int*): Unit = {
    terminal.add.expects(where { (message: String) => {
        codePoints == message.map(c => c.toInt)
    } })
  }

  private class InteractiveCommand extends VirtualCommand {
    override def name: String = "int"

    override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*): Either[IOError, VirtualProcess] = {
      var _running = true
      var count = 5

      Right(new VirtualProcess {
        override def update(): Unit = {
          count = count - 1
          if (count == 0) {
            _running = false
          }
        }

        override def running: Boolean = _running
      })
    }

    override def completion(line: String, shell: VirtualShell): Seq[Completion] = Seq.empty
  }

  private class FakeScheduler extends Scheduler {
    private var count = 0
    def init(): Unit = {
      count = 2
    }
    override def run(callback: Double => Unit): Unit = {
      count = count -1
      if (count >= 0) {
        callback.apply(0.0)
      }
    }
  }

  private def assertError(result: Either[IOError, Unit], message: String): Unit = {
    result match {
      case Left(error) => assert(error.message == message)
      case _ => fail("Should return an error.")
    }
  }

  private def expectPrompt(terminal : Terminal,  prompt : Boolean = true): Unit = {
    if (prompt) {
      terminal.add.expects(where { (message: String) => message.contains(VirtualShellImpl.formatUserPrompt("guest")) })
    } else {
      terminal.add.expects(where { (message: String) => message.contains(VirtualShellImpl.formatUserPrompt("guest")) }).never()
    }
  }
}
