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

  def createCommandFile(folder: VirtualFolder, command: VirtualCommand): Either[IOError, VirtualFile] = {
    for {
      file <- folder.touch(command.getName).right
      _ <- file.setExecutable().right
      _ <- (file.content = command).right
    } yield {
      file
    }
  }

  def getCommand(file: VirtualFile): Either[IOError, VirtualCommand] =
    file.content match {
      case Left(error) => Left(error)
      case Right(command: VirtualCommand) => Right(command)
      case _ => "File is not a command.".ioErrorE
    }

  def findCommand(command: String, currentFolder: VirtualFolder) : Option[VirtualFile] = {
    val first: Option[VirtualFile] = path
      .map(folder => {
        folder.findFile(command).right.get
      })
      .flatMap(_.toList)
      .headOption

    first.orElse(currentFolder.findFile(command).right.get)
  }

}
