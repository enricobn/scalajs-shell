package org.enricobn.shell

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.IOError.*
import org.enricobn.vfs.utils.Utils
import org.enricobn.vfs.{Authentication, IOError, VirtualFile, VirtualFolder}

object VirtualCommandOperations {

  def createCommandFile(folder: VirtualFolder, command: VirtualCommand)(implicit authentication: Authentication): Either[IOError, VirtualFile] = {
    for {
      file <- folder.touch(command.name)
      _ <- file.setExecutable
      _ <- file.setContent(command)
    } yield {
      file
    }
  }

  def getCommand(file: VirtualFile)(implicit authentication: Authentication): Either[IOError, VirtualCommand] =
    file.getContent match {
      case Left(error) => Left(error)
      case Right(command: VirtualCommand) => Right(command)
      case _ => "File is not a command.".ioErrorE
    }

  def createCommandFiles(folder: VirtualFolder, commands: VirtualCommand*)(implicit rootAuthentication: Authentication): Either[IOError, List[VirtualFile]] =
    Utils.lift(commands.map(createCommandFile(folder, _)))

}

/**
  * Created by enrico on 12/4/16.
  */
trait VirtualCommand {

  def name: String

  def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*) : Either[IOError, VirtualProcess]

  def completion(line: String, shell: VirtualShell): Seq[Completion]

  override def toString: String = "[executable file]"

}
