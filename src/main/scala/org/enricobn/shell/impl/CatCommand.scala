package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.VirtualFolder

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CatCommand")
class CatCommand extends VirtualCommand {
  override def getName: String = "cat"

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*)  = {
    if (args.isEmpty) {
      "cat: illegal argument".ioErrorE
    } else {
      val currentFolder: VirtualFolder = shell.currentFolder
      currentFolder.findFile(args(0)) match {
        case Left(error) => error.message.ioErrorE
        case Right(Some(file)) =>
          file.content match {
            case Left(error) => error.message.ioErrorE
            case Right(c) =>
              Right({
                shellOutput.write(c.toString)
                shellOutput.write(VirtualShell.CRLF)
                shellOutput.flush()
              })
          }
        case _ => s"cat: ${args(0)}: No such file or directory".ioErrorE
      }
    }
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    val parsedLine = new ParsedLine(line)
    val start = parsedLine.lastArgument.getOrElse("")

    currentFolder.files match {
      case Left(error) => Seq.empty // TODO error
      case Right(fs) =>
        fs
          .map(_.name)
          .filter(_.startsWith(start))
          .toSeq
    }
  }
}
