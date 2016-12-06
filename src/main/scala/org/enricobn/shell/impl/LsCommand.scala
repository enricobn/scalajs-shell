package org.enricobn.shell.impl

import org.enricobn.shell.VirtualCommand
import org.enricobn.terminal.Terminal
import org.enricobn.vfs._

/**
  * Created by enrico on 12/4/16.
  */
object LsCommand {
  private def toString(permissions: VirtualPermissions): String = {
    toString(permissions.owner) + " " + toString(permissions.group) + " " + toString(permissions.others)
  }

  private def toString(permission: VirtualPermission): String =
    (if (permission.execute) "x" else "-") +
    (if (permission.read) "r" else "-") +
    (if (permission.write) "w" else "-")
}

class LsCommand extends VirtualCommand {
  def getName: String = "ls"

  @throws[VirtualIOException]
  def run(shell: VirtualShell, terminal: Terminal, args: String*) {
    val currentFolder: VirtualFolder = shell.getCurrentFolder
    currentFolder.getFolders.foreach(folder => print(terminal, folder))
    currentFolder.getFiles.foreach(file => print(terminal, file))
  }

  private def getAttributes(node: VirtualNode): String = {
    LsCommand.toString(node.getPermissions)
  }

  private def print(terminal: Terminal, node: VirtualNode) {
    terminal.add(node.getName + "\t\t" + node.getOwner + "\t\t" + getAttributes(node) + "\n")
  }

}
