package org.enricobn.shell.impl

import org.enricobn.shell.*
import org.enricobn.vfs.IOError.*
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}

import scala.reflect.ClassTag
import scala.scalajs.js.annotation.JSExportTopLevel

private object MkdirCommandArguments {

  val FOLDER: NewFolderArgument = NewFolderArgument("folder", required = true, (folder, shell) => {
    folder.getCurrentUserPermission(shell.authentication).toOption.get.read
  })

}

/**
  * Created by enrico on 12/5/16.
  */
@JSExportTopLevel(name = "MkdirCommand")
object MkdirCommand extends VirtualCommandAbstract("mkdir", MkdirCommandArguments.FOLDER) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {

    args match {
      case Seq(folderAndName: (VirtualFolder, String)) =>
        folderAndName._1.mkdir(folderAndName._2).flatMap(_ => Right(new VirtualProcess()))
      case _ => "mkdir: illegal argument".ioErrorE
    }

  }

}
