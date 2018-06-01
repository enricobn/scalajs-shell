package org.enricobn.shell.impl

import org.enricobn.shell.{RunContext, ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.terminal.TerminalColors
import org.enricobn.vfs._
import org.enricobn.vfs.IOError._

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/4/16.
  */
object LsCommand {
  // TODO filter?
  private val FOLDER = FolderArgument("folder", false)

  private def toString(permissions: VirtualPermissions): String = {
    toString(permissions.owner) + " " + toString(permissions.group) + " " + toString(permissions.others)
  }

  private def toString(permission: VirtualPermission): String =
    (if (permission.read) "r" else "-") +
    (if (permission.write) "w" else "-") +
    (if (permission.execute) "x" else "-")
}

import LsCommand._

@JSExport(name = "LsCommand")
class LsCommand extends VirtualCommandAbstract("ls", FOLDER) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
  : Either[IOError, RunContext] = {

    val errorOrFolder = args match {
      case Seq(folder: VirtualFolder) => Right(folder)
      case Seq() => Right(shell.currentFolder)
      case _ => "ls: illegal argument".ioErrorE
    }

    errorOrFolder match {
      case Left(error) => Left(error)
      case Right(folder) =>
        folder.folders.right.get.foreach(f => print(shellOutput, f))
        folder.files.right.get.foreach(file => print(shellOutput, file))
        Right(new RunContext())
    }

  }

  private def getAttributes(node: VirtualNode): String = {
    LsCommand.toString(node.permissions)
  }

  private def print(out: ShellOutput, node: VirtualNode) {
    var s = getAttributes(node) + "  " + "%1$-10s".format(node.owner) + "  "

    if (node.isInstanceOf[VirtualFolder]) {
      val colored = new TerminalColors()
      colored.blue.add(node.name).end
      s += (colored + " ")
    } else {
      s += node.name
    }
    out.write(s + VirtualShell.CRLF)
    out.flush()
  }

}
