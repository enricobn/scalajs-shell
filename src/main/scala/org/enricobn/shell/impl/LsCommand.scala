package org.enricobn.shell.impl

import org.enricobn.shell.VirtualCommand
import org.enricobn.terminal.Terminal
import org.enricobn.vfs._

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/4/16.
  */
object LsCommand {
  private def toString(permissions: VirtualPermissions): String = {
    toString(permissions.owner) + " " + toString(permissions.group) + " " + toString(permissions.others)
  }

  private def toString(permission: VirtualPermission): String =
    (if (permission.read) "r" else "-") +
    (if (permission.write) "w" else "-") +
    (if (permission.execute) "x" else "-")
}

@JSExport(name = "LsCommand")
class LsCommand extends VirtualCommand {
  def getName: String = "ls"

  @throws[VirtualIOException]
  def run(shell: VirtualShell, terminal: Terminal, args: String*) {
    val currentFolder: VirtualFolder = shell.currentFolder
    currentFolder.folders.foreach(folder => print(terminal, folder))
    currentFolder.files.foreach(file => print(terminal, file))
  }

  private def getAttributes(node: VirtualNode): String = {
    LsCommand.toString(node.permissions)
  }

  private def print(terminal: Terminal, node: VirtualNode) {
    terminal.add(node.name + "\t\t" + node.owner + "\t\t" + getAttributes(node) + VirtualShell.CRLF)
    terminal.flush()
  }

}
