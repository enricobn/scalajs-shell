package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualProcess}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "TouchCommand")
object TouchCommand extends VirtualCommandAbstract("touch", NewFileArgument("file", required = true)) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {
    args match {
      case Seq(folderAndName: (VirtualFolder, String)) =>
        folderAndName._1.touch(folderAndName._2).right.flatMap(_ => Right(new VirtualProcess()))
      case _ => "touch: illegal argument".ioErrorE
    }
  }

}
