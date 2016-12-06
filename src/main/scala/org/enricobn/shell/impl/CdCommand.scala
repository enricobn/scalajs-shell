package org.enricobn.shell.impl

import org.enricobn.shell.VirtualCommand
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.{VirtualFolder, VirtualIOException}

/**
  * Created by enrico on 12/5/16.
  */
class CdCommand extends VirtualCommand {
  override def getName: String = "cd"

  @throws[VirtualIOException]
  override def run(shell: VirtualShell, terminal: Terminal, args: String*) {
    val folder: String = args(0)

    var currentFolder: VirtualFolder = null
    try {
      currentFolder = shell.getCurrentFolder.resolveFolder(folder)
    }
    catch {
      case e: VirtualIOException =>
        throw new VirtualIOException("cd: " + e.getMessage, e)
    }
    shell.setCurrentFolder(currentFolder)
  }
}
