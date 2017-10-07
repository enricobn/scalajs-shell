package org.enricobn.shell.impl

import org.enricobn.shell.{RunContext, ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{IOError, VirtualFile, VirtualFolder}

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CatCommand")
class CatCommand extends VirtualCommand {
  private val arguments = new VirtualCommandArguments(
    FileArgument("file", true)
  )

  override def getName: String = "cat"

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*)  = {
    arguments.parse(shell.currentFolder, getName, args:_*) match {
      case Left(error) => Left(IOError(error))
      case Right(Seq(file: VirtualFile)) =>
        file.content match {
          case Left(error) => error.message.ioErrorE
          case Right(c) =>
            Right({
              shellOutput.write(c.toString)
              shellOutput.write(VirtualShell.CRLF)
              shellOutput.flush()
              new RunContext()
            })
        }
      case _ => "cat: illegal argument".ioErrorE
    }
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] =
    arguments.complete(currentFolder, line)

}
