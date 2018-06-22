package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.enricobn.vfs.Authentication
import org.enricobn.vfs.impl.UnixLikeInMemoryFS
import org.enricobn.vfs.utils.Utils.RightBiasedEither

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
    val fs = UnixLikeInMemoryFS("root").right.get
    implicit val rootAuthentication: Authentication = fs.vum.logRoot("root").right.get
    val rootFolder = fs.root

    fs.vum.addUser("guest", "guest")

    val shellE = for {
      homeGuest <- rootFolder.resolveFolderOrError("/home/guest")
      text <- homeGuest.touch("text.txt")
      _ <- text.setContent("Hello\nWorld").toLeft(None)
      _ <- text.chmod(666).toLeft(None).right
      authentication <- fs.vum.logUser("guest", "guest")

      shell = UnixLikeVirtualShell(fs, terminal, homeGuest, authentication)

      _ <- shell.context.createCommandFiles(fs.bin, new LsCommand(), new CdCommand(), new CatCommand())
      _ = List(fs.bin, fs.usrBin).foreach(shell.context.addToPath)
    } yield shell

    shellE match {
      case Left(error) =>
        terminal.add(error.message + CRLF)
        terminal.flush()
        null
      case Right(shell) =>
        shell
    }
  }
}
