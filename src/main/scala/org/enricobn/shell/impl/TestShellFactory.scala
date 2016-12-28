package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/19/16.
  */
@JSExport("TestShellFactory")
object TestShellFactory {
  @JSExport
  def create(terminal: Terminal) : VirtualShell = {
    val vum = new VirtualUsersManagerImpl("root")
    val fs = new InMemoryFS(vum)
    val rootFolder = fs.root
    val context = new VirtualShellContextImpl()

    vum.addUser("guest", "guest")

    val job = for {
      bin <- rootFolder.mkdir("bin").right
      usr <- rootFolder.mkdir("usr").right
      usrBin <- usr.mkdir("bin").right
      home <- rootFolder.mkdir("home").right
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
        null
      case Right(j) =>
        j.path.foreach(context.addToPath)
        vum.logUser("guest", "guest")
        new VirtualShell(terminal, vum, context, j.currentFolder)
    }
  }
}
