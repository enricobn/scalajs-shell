package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs.Authentication
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerImpl}
import org.enricobn.vfs.inmemory.InMemoryFS

import scala.scalajs.js.annotation.JSExport

// to access members of structural types (new {}) without warnings
import org.enricobn.terminal.Terminal._

import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/19/16.
  */
@JSExport("TestShellFactory")
object TestShellFactory {
  @JSExport
  def create(terminal: Terminal) : VirtualShell = {
    val vum = new VirtualUsersManagerImpl("root")
    val vsm = new VirtualSecurityManagerImpl(vum)
    val fs = new InMemoryFS(vum, vsm)
    val rootFolder = fs.root
    val context = new VirtualShellContextImpl()
    implicit val rootAuthentication: Authentication = vum.logRoot("root").right.get

    vum.addUser("guest", "guest")

    val job = for {
      bin <- rootFolder.mkdir("bin").right
      usr <- rootFolder.mkdir("usr").right
      usrBin <- usr.mkdir("bin").right
      home <- rootFolder.mkdir("home").right
      homeGuest <- home.mkdir("guest").right
      text <- homeGuest.touch("text.txt").right
      _ <- text.setContent("Hello\nWorld").toLeft(None).right
      _ <- text.chmod(666).toLeft(None).right
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
        terminal.add(error.message + CRLF)
        terminal.flush()
        null
      case Right(j) =>
        j.path.foreach(context.addToPath)
        val authentication = vum.logUser("guest", "guest").right.get
        new VirtualShell(terminal, vum, vsm, context, j.currentFolder, authentication)
    }
  }
}
