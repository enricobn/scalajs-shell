package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualProcess}
import org.enricobn.vfs.IOError.*
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}

import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * Created by enrico on 12/5/16.
  */
@JSExportTopLevel(name = "TouchCommand")
object TouchCommand extends VirtualCommandAbstract("touch", NewFileArgument("file", required = true)) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {
    args match {
      case Seq(folderAndName: (VirtualFolder, String)) =>
        folderAndName._1.touch(folderAndName._2).flatMap(_ => Right(new VirtualProcess()))
      case _ => "touch: illegal argument".ioErrorE
    }
  }

}
