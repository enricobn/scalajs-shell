package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs.VirtualFolder
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

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

    var currentFolder: VirtualFolder = fs.root

    val bin = currentFolder.mkdir("bin")

    val usr = currentFolder.mkdir("usr")
    val usrBin = usr.mkdir("bin")
    currentFolder = currentFolder.mkdir("home")
    currentFolder = currentFolder.mkdir("guest")
    val text = currentFolder.touch("text.txt")
    text.chmod(666)

    val virtualShell = new VirtualShell(term, vum, currentFolder)
    virtualShell.createCommandFile(bin, new LsCommand())
    virtualShell.createCommandFile(bin, new CdCommand())
    virtualShell.createCommandFile(bin, new CatCommand())
    virtualShell.addToPath(bin)
    virtualShell.addToPath(usrBin)

    vum.logUser("guest", "guest")
    text.content = "Hello\nWorld"

    (term.add _).expects(where {message: String => message.contains("/home/guest")})
    (term.flush _).expects().anyNumberOfTimes()
    (term.onInput _).expects(*).anyNumberOfTimes()
    (term.removeOnInputs _).expects().anyNumberOfTimes()

    virtualShell.start()

    new {
      val shell = virtualShell
      val terminal = term
    }
  }

  "ls" should "show text.txt" in {
    val f = fixture

    (f.terminal.add _).expects(where {
      message: String => message.contains("text.txt") && message.contains("rw- rw- rw-")
    })
//    (f.terminal.flush _).expects()

    f.shell.run("ls")
  }

  "cd" should "show bin, home and usr" in {
    val f = fixture

    (f.terminal.add _).expects(where {
      message: String => message.contains("bin") && message.contains("rwx rwx r-x")
    })
//    (f.terminal.flush _).expects()

    (f.terminal.add _).expects(where {
      message: String => message.contains("home") && message.contains("rwx rwx r-x")
    })
//    (f.terminal.flush _).expects()

    (f.terminal.add _).expects(where {
      message: String => message.contains("usr") && message.contains("rwx rwx r-x")
    })
//    (f.terminal.flush _).expects()

    f.shell.run("cd", "/")
    f.shell.run("ls")
  }
}
