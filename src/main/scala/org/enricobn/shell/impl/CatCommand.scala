package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.vfs.{VirtualFile, VirtualFolder, VirtualIOException}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CatCommand")
class CatCommand extends VirtualCommand {
  override def getName: String = "cat"

  @throws[VirtualIOException]
  override def run(shell: VirtualShell, in: ShellInput, out: ShellOutput, args: String*) {
    if (args.isEmpty) {
      throw new VirtualIOException("cat: illegal argument")
    }
    val currentFolder: VirtualFolder = shell.currentFolder
    val file: VirtualFile = currentFolder.findFileOrThrow(args(0))
    out.write(file.content.toString)
    out.write(VirtualShell.CRLF)
    out.flush()
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    val parsedLine = new ParsedLine(line)
    val start = parsedLine.lastArgument.getOrElse("")

    currentFolder.files
      .map(_.name)
      .filter(_.startsWith(start))
      .toSeq
  }
}
