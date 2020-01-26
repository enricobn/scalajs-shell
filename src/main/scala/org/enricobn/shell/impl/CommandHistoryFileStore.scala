package org.enricobn.shell.impl

import org.enricobn.shell.CommandHistoryStore
import org.enricobn.vfs.{Authentication, IOError}

class CommandHistoryFileStore(private val shell: VirtualShell) extends CommandHistoryStore {

  private def file = {
    implicit val authentication: Authentication = shell.authentication

    for {
      homeFolder <- shell.homeFolder.right
      alreadyPresent <- homeFolder.findFile(".history").right
      file <- if (alreadyPresent.isDefined) {
        Right(alreadyPresent.get).right
      } else {
        homeFolder.createFile(".history", StringList()).right
      }
    } yield file
  }

  override def add(command: String): Either[IOError, Unit] =
    for {
      f <- file.right
      c <- content.right
      result <- f.setContent(c( _:+ command))(shell.authentication).right
    } yield result

  override def apply(i: Int): Either[IOError, String] =
    for {
      c <- content.right
    } yield c.value(i)

  override def length: Either[IOError, Int] =
    for {
      c <- content.right
    } yield c.value.size

  override def lastOption: Either[IOError, Option[String]] =
    for {
      c <- content.right
    } yield c.value.lastOption

  override def removeHead: Either[IOError, Unit] =
    for {
      f <- file.right
      c <- content.right
      result <- f.setContent(c.value.tail)(shell.authentication).right
    } yield result

  private def content =
    for {
      f <- file.right
      c <- f.contentAs(classOf[StringList])(shell.authentication).right
    } yield c

}
