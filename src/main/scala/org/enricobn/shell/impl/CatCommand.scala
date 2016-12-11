package org.enricobn.shell.impl

import org.enricobn.shell.VirtualCommand
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.{VirtualFile, VirtualFolder, VirtualIOException}

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CatCommand")
class CatCommand extends VirtualCommand {
  override def getName: String = "cat"

  @throws[VirtualIOException]
  override def run(shell: VirtualShell, terminal: Terminal, args: String*) {
    if (args.isEmpty) {
      throw new VirtualIOException("cat: illegal argument")
    }
    val currentFolder: VirtualFolder = shell.currentFolder
    val file: VirtualFile = currentFolder.findFileOrThrow(args(0))
    terminal.add(file.content.toString)
    terminal.add(VirtualShell.CRLF)
    terminal.flush
  }
}
