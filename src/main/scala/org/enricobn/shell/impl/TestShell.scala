package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs.{VirtualFS, VirtualUsersManager}

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/19/16.
  */
@JSExportAll
@JSExport("TestShell")
class TestShell(terminal: Terminal, vum: VirtualUsersManager, fs: VirtualFS) extends VirtualShell(terminal, vum, fs.root) {
  vum.addUser("guest", "guest")

  private val job = for {
    bin <- currentFolder.mkdir("bin").right
    usr <- currentFolder.mkdir("usr").right
    usrBin <- usr.mkdir("bin").right
    home <- currentFolder.mkdir("home").right
    homeGuest <- home.mkdir("guest").right
    text <- homeGuest.touch("text.txt").right
    contentEff <- (text.content = "Hello\nWorld").right
    chmodEff <- text.chmod(666).right
    _ <- createCommandFile(bin, new LsCommand()).right
    _ <- createCommandFile(bin, new CdCommand()).right
    _ <- createCommandFile(bin, new CatCommand()).right
  } yield new {
    val path = List(bin, usrBin)
    val effects = List(contentEff, chmodEff)
    val textFile = text
    val currentFolder = homeGuest
  }

  job match {
    case Left(error) =>
      terminal.add(error.message + VirtualShell.CRLF)
      terminal.flush()
    case Right(j) =>
      j.path.foreach(addToPath(_))
      j.effects.foreach(_.apply())
      currentFolder = j.currentFolder
  }

  vum.logUser("guest", "guest")

  /*
  val bin = currentFolder.mkdir("bin").right.get

  val usr = currentFolder.mkdir("usr").right.get
  val usrBin = usr.mkdir("bin").right.get
  currentFolder = currentFolder.mkdir("home").right.get
  currentFolder = currentFolder.mkdir("guest").right.get
  val text = currentFolder.touch("text.txt").right.get
  text.chmod(666).right.get.apply()

  createCommandFile(bin, new LsCommand())
  createCommandFile(bin, new CdCommand())
  createCommandFile(bin, new CatCommand())
  addToPath(bin)
  addToPath(usrBin)

  vum.logUser("guest", "guest")
  (text.content = "Hello\nWorld").right.get.apply()
*/
}
