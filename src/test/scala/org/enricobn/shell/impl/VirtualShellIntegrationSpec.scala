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

    val bin = currentFolder.mkdir("bin").right.get

    val usr = currentFolder.mkdir("usr").right.get
    val usrBin = usr.mkdir("bin").right.get
    currentFolder = currentFolder.mkdir("home").right.get
    currentFolder = currentFolder.mkdir("guest").right.get
    val text = currentFolder.touch("text.txt").right.get
    text.chmod(666)

    val context = new VirtualShellContext()
    context.createCommandFile(bin, new LsCommand())
    context.createCommandFile(bin, new CdCommand())
    context.createCommandFile(bin, new CatCommand())
    context.addToPath(bin)
    context.addToPath(usrBin)
    val virtualShell = new VirtualShell(term, vum, context, currentFolder)

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

    f.shell.run("ls")
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

    f.shell.run("cd", "/")
    f.shell.run("ls")
  }
}
