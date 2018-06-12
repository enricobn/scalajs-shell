package org.enricobn.shell.impl

import org.enricobn.shell.{VirtualCommand, VirtualShellContext}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by enrico on 12/23/16.
  */
class VirtualShellContextImpl extends VirtualShellContext {
  val path = new ArrayBuffer[VirtualFolder]()

  def addToPath(folder: VirtualFolder) {
    path += folder
  }

  // TODO I don't like it here, has nothing to do with context!
  def createCommandFile(folder: VirtualFolder, command: VirtualCommand)(implicit authentication: Authentication): Either[IOError, VirtualFile] = {
    for {
      file <- folder.touch(command.name).right
      _ <- file.setExecutable.toLeft(None).right
      _ <- file.setContent(command).toLeft(None).right
    } yield {
      file
    }
  }

  // TODO I don't like it here, has nothing to do with context!
  def getCommand(file: VirtualFile)(implicit authentication: Authentication): Either[IOError, VirtualCommand] =
    file.getContent match {
      case Left(error) => Left(error)
      case Right(command: VirtualCommand) => Right(command)
      case _ => "File is not a command.".ioErrorE
    }

  // TODO I don't like it here, has something about context, but I think its better to let context hold only
  // the state.
  def findCommand(command: String, currentFolder: VirtualFolder)(implicit authentication: Authentication) : Option[VirtualFile] = {
    val first: Option[VirtualFile] = path
      .map(folder => {
        folder.findFile(command).right.get
      })
      .flatMap(_.toList)
      .headOption

    first.orElse(currentFolder.findFile(command).right.get)
  }

}
