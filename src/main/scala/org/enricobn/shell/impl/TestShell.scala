package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs.{VirtualFS, VirtualUsersManager}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/19/16.
  */
@JSExportAll
@JSExport("TestShell")
class TestShell(terminal: Terminal, vum: VirtualUsersManager, fs: VirtualFS, context: VirtualShellContext)
    extends VirtualShell(terminal, vum, context, fs.root) {
  vum.addUser("guest", "guest")

  private val job = for {
    bin <- currentFolder.mkdir("bin").right
    usr <- currentFolder.mkdir("usr").right
    usrBin <- usr.mkdir("bin").right
    home <- currentFolder.mkdir("home").right
    homeGuest <- home.mkdir("guest").right
    text <- homeGuest.touch("text.txt").right
    _ <- (text.content = "Hello\nWorld").right
    _ <- text.chmod(666).right
    _ <- context.createCommandFile(bin, new LsCommand()).right
    _ <- context.createCommandFile(bin, new CdCommand()).right
    _ <- context.createCommandFile(bin, new CatCommand()).right
  } yield new {
    val path = List(bin, usrBin)
    val textFile = text
    val currentFolder = homeGuest
  }

  job match {
    case Left(error) =>
      terminal.add(error.message + VirtualShell.CRLF)
      terminal.flush()
    case Right(j) =>
      j.path.foreach(context.addToPath(_))
      currentFolder = j.currentFolder
  }

  vum.logUser("guest", "guest")
}
