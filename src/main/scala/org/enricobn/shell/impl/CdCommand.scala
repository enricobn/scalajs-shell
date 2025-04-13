package org.enricobn.shell.impl

import org.enricobn.shell.*
import org.enricobn.vfs.IOError.*
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}

import scala.scalajs.js.annotation.JSExportTopLevel

private object CdCommandArguments {

  val FOLDER: FolderArgument = FolderArgument("folder", required = false, (folder, shell) => folder.getCurrentUserPermission(shell.authentication).toOption.get.execute)

}

/**
  * Created by enrico on 12/5/16.
  */
@JSExportTopLevel(name = "CdCommand")
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
