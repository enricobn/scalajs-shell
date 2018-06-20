package org.enricobn.shell.impl

import org.enricobn.shell.VirtualShellProfile
import org.enricobn.vfs.{Authentication, IOError}

class VirtualShellFileProfile(private val shell: VirtualShell) extends VirtualShellProfile {

  private def file = {
    implicit val authentication: Authentication = shell.authentication

    for {
      homeFolder <- shell.homeFolder.right
      alreadyPresent <- homeFolder.findFile(".profile").right
      file <- if (alreadyPresent.isDefined) {
        Right(alreadyPresent.get).right
      } else {
        homeFolder.createFile(".profile", StringMap()).right
      }
    } yield file
  }

  def add(key: String, value: String): Either[IOError, Unit] =
    for {
      c <- content.right
      f <- file.right
      result <- f.setContent(c(_ + (key -> value)))(shell.authentication).toLeft(()).right
    } yield result

  def apply(key: String): Either[IOError, Option[String]] =
    for {
      c <- content.right
    } yield c.value.get(key)

  private def content =
    for {
      f <- file.right
      c <- f.contentAs(classOf[StringMap])(shell.authentication).right
    } yield c

}
