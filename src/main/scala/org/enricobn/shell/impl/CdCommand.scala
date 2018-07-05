package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}

import scala.scalajs.js.annotation.JSExport

object CdCommand {

  val FOLDER = FolderArgument("folder", false, (folder, shell) => folder.getCurrentUserPermission(shell.authentication).right.get.execute)

}

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CdCommand")
class CdCommand extends VirtualCommandAbstract("cd", CdCommand.FOLDER) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {

    val errorOrFolder = args match {
      case Seq(folder: VirtualFolder) => Right(folder)
      case Seq() => shell.homeFolder
      case _ => "cd: illegal argument".ioErrorE
    }

    errorOrFolder match {
      case Left(error) => error.message.ioErrorE
      case Right(folder) =>
          shell.currentFolder = folder
          Right(new VirtualProcess())
      }

  }

}
