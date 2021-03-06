package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}

import scala.scalajs.js.annotation.JSExport

private object CdCommandArguments {

  val FOLDER = FolderArgument("folder", required = false, (folder, shell) => folder.getCurrentUserPermission(shell.authentication).right.get.execute)

}

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CdCommand")
object CdCommand extends VirtualCommandAbstract("cd", CdCommandArguments.FOLDER) {

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
