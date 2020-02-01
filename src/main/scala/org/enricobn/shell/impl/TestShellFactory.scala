package org.enricobn.shell.impl

import org.enricobn.shell.VirtualCommandOperations
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.impl.{VirtualSecurityManagerImpl, VirtualUsersManagerFileImpl}
import org.enricobn.vfs.inmemory.InMemoryFS
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs.{Authentication, VirtualFileWithContent, VirtualPath}

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
    val _fs = InMemoryFS(
      {VirtualUsersManagerFileImpl(_, "root").right.get},
      {(_, vum) => new VirtualSecurityManagerImpl(vum)})
    val fs = UnixLikeInMemoryFS(_fs, "root").right.get
    implicit val rootAuthentication: Authentication = fs.vum.logRoot("root").right.get

    fs.vum.addUser("guest", "guest", "guest")

    val shellE = for {
      homeGuestPath <- VirtualPath.absolute("home", "guest", "text.txt")
      homeGuestFC = new VirtualFileWithContent(classOf[String], fs, homeGuestPath)
      homeGuest <- VirtualPath.absolute("home", "guest").flatMap(_.toFolder(fs))
      _ <- homeGuestFC.setContent("Hello\nWorld")
      _ <- homeGuestFC.file.flatMap(_.chmod(666))
      authentication <- fs.vum.logUser("guest", "guest")

      shell = UnixLikeVirtualShell(fs, terminal, homeGuest, authentication)

      _ <- VirtualCommandOperations.createCommandFiles(fs.bin, LsCommand, CdCommand, CatCommand)
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
