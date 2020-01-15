package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualProcess}
import org.enricobn.terminal.Terminal._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{Authentication, IOError, VirtualFile}

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CatCommand")
object CatCommand extends VirtualCommandAbstract("cat", FileArgument("file", required = true)) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {
    val errorOrFile = args match {
      case Seq(file: VirtualFile) => Right(file)
      case _ => "cat: illegal argument".ioErrorE
    }

    errorOrFile match {
      case Left(error) => Left(error)
      case Right(file) =>
        file.getContent match {
          case Left(error) => error.message.ioErrorE
          case Right(c) =>
            shellOutput.write(c.toString)
            shellOutput.write(CRLF)
            shellOutput.flush()

            Right(new VirtualProcess())
        }
    }
  }

}
