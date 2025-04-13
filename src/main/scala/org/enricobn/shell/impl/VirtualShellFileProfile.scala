package org.enricobn.shell.impl

import org.enricobn.shell.VirtualShellProfile
import org.enricobn.vfs.*

class VirtualShellUserProfile(private val shell: VirtualShell) extends VirtualShellFileProfile(() => shell.authentication) {

  def file: Either[IOError, VirtualFile] = {
    implicit val authentication: Authentication = shell.authentication

    for {
      homeFolder <- shell.homeFolder
      alreadyPresent <- homeFolder.findFile(".profile")
      file <- if (alreadyPresent.isDefined) {
        Right(alreadyPresent.get)
      } else {
        homeFolder.createFile(".profile", StringMap())
      }
    } yield file
  }

}

class VirtualShellGlobalProfile(private val fs: VirtualFS, private val authentication: () => Authentication) extends VirtualShellFileProfile(authentication) {

  def file: Either[IOError, VirtualFile] = {
    for {
      etcPath <- VirtualPath.of("etc")
      etcFolder <- etcPath.toFolder(fs.root)(authentication())
      alreadyPresent <- etcFolder.findFile("profile")(authentication())
      file <- if (alreadyPresent.isDefined) {
        Right(alreadyPresent.get)
      } else {
        etcFolder.createFile("profile", StringMap())(authentication())
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
      f <- file
      c <- f.contentAs(classOf[StringMap])(authentication())
    } yield c

  def add(key: String, value: String): Either[IOError, Unit] =
    for {
      c <- content
      f <- file
      result <- f.setContent(c(_ + (key -> value)))(authentication())
    } yield result

  def apply(key: String): Either[IOError, Option[String]] =
    for {
      c <- content
    } yield c.value.get(key)

}
