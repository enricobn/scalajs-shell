package org.enricobn.shell.impl

import org.enricobn.shell.VirtualShellProfile
import org.enricobn.vfs.utils.Utils.RightBiasedEither
import org.enricobn.vfs._

class VirtualShellUserProfile(private val shell: VirtualShell) extends VirtualShellFileProfile(() => shell.authentication) {

  def file: Either[IOError, VirtualFile] = {
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

}

class VirtualShellGlobalProfile(private val fs: VirtualFS, private val authentication: () => Authentication) extends VirtualShellFileProfile(authentication) {

  def file: Either[IOError, VirtualFile] = {
    for {
      etcPath <- VirtualPath.of("etc")
      etcFolder <- etcPath.toFolder(fs.root)(authentication()).right
      alreadyPresent <- etcFolder.findFile("profile")(authentication()).right
      file <- if (alreadyPresent.isDefined) {
        Right(alreadyPresent.get).right
      } else {
        etcFolder.createFile("profile", StringMap())(authentication()).right
      }
    } yield file
  }

}

abstract class VirtualShellFileProfile(authentication: () => Authentication) extends VirtualShellProfile {

  def file : Either[IOError, VirtualFile]

  override def keys: Either[IOError, Set[String]] =
    content match {
      case Right(c) => Right(c.value.keys.toSet)
      case Left(e) => Left(e)
    }

  def content: Either[IOError, StringMap] =
    for {
      f <- file.right
      c <- f.contentAs(classOf[StringMap])(authentication()).right
    } yield c

  def add(key: String, value: String): Either[IOError, Unit] =
    for {
      c <- content.right
      f <- file.right
      result <- f.setContent(c(_ + (key -> value)))(authentication()).right
    } yield result

  def apply(key: String): Either[IOError, Option[String]] =
    for {
      c <- content.right
    } yield c.value.get(key)

}
