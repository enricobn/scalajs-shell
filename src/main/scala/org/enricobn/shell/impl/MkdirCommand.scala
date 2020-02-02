package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}

import scala.scalajs.js.annotation.JSExport

private object MkdirCommandArguments {

  val FOLDER = NewFolderArgument("folder", required = true, (folder, shell) => {
    folder.getCurrentUserPermission(shell.authentication).right.get.read
  })

}

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "MkdirCommand")
object MkdirCommand extends VirtualCommandAbstract("mkdir", MkdirCommandArguments.FOLDER) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {

    args match {
      case Seq(folderAndName: (VirtualFolder, String)) =>
        folderAndName._1.mkdir(folderAndName._2).right.flatMap(_ => Right(new VirtualProcess()))
      case _ => "mkdir: illegal argument".ioErrorE
    }

  }

}
