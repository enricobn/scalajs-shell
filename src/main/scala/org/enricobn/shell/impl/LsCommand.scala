package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualCommand}
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

  override def run(shell: VirtualShell, in: ShellInput, out: ShellOutput, args: String*) = {
    val currentFolder: VirtualFolder = shell.currentFolder

    Right(() => {
      currentFolder.folders.right.get.foreach(folder => print(out, folder))
      currentFolder.files.right.get.foreach(file => print(out, file))
    })
  }

  private def getAttributes(node: VirtualNode): String = {
    LsCommand.toString(node.permissions)
  }

  private def print(out: ShellOutput, node: VirtualNode) {
    out.write(node.name + "\t\t" + node.owner + "\t\t" + getAttributes(node) + VirtualShell.CRLF)
    out.flush()
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    Seq.empty
  }
}
