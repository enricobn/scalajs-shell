package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualProcess}
import org.enricobn.terminal.Terminal._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/4/16.
  */

@JSExport(name = "LsCommand")
object LsCommand extends VirtualCommandAbstract("ls", FolderArgument("folder", required = false)) {

  override def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])
                        (implicit authentication: Authentication)
  : Either[IOError, VirtualProcess] = {

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
        Right(new VirtualProcess())
    }

  }

  private def getAttributes(node: VirtualNode): String = {
    LsCommand.toString(node.permissions)
  }

  private def print(out: ShellOutput, node: VirtualNode) {
    var s = getAttributes(node) + "  " + "%1$-10s".format(node.owner) + "  " + "%1$-10s".format(node.group) + "  "

    if (node.isInstanceOf[VirtualFolder]) {
      s += Console.BLUE + node.name + Console.RESET
    } else {
      s += node.name
    }
    out.write(s + CRLF)
    out.flush()
  }

  private def toString(permissions: VirtualPermissions): String = {
    toString(permissions.owner) + " " + toString(permissions.group) + " " + toString(permissions.others)
  }

  private def toString(permission: VirtualPermission): String =
    (if (permission.read) "r" else "-") +
      (if (permission.write) "w" else "-") +
      (if (permission.execute) "x" else "-")

}
