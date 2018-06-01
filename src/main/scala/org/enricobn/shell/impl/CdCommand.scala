package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{IOError, VirtualFile, VirtualFolder}

import scala.scalajs.js.annotation.JSExport

object CdCommand {

  val FOLDER = FolderArgument("folder", false, _.getCurrentUserPermission.execute)

}

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CdCommand")
class CdCommand extends VirtualCommandAbstract("cd", CdCommand.FOLDER) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
  : Either[IOError, RunContext] = {

    val errorOrFolder = args match {
      case Seq(folder: VirtualFolder) => Right(folder)
      case Seq() => shell.homeFolder
      case _ => "cd: illegal argument".ioErrorE
    }

    errorOrFolder match {
      case Left(error) => error.message.ioErrorE
      case Right(folder) =>
          shell.currentFolder = folder
          Right(new RunContext())
      }

  }

}
