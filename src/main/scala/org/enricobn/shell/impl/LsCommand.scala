package org.enricobn.shell.impl

import org.enricobn.shell.{RunContext, ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.vfs._
import org.enricobn.vfs.IOError._

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

  private val arguments = new VirtualCommandArguments(
    // TODO filter?
    FolderArgument("folder", false)
  )

  def getName: String = "ls"

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*) = {
    val errorOrFolder = arguments.parse(shell.currentFolder, getName, args: _*) match {
      case Left(error) => Left(IOError(error))
      case Right(Seq(folder: VirtualFolder)) => Right(folder)
      case Right(Seq()) => Right(shell.currentFolder)
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
    out.write(getAttributes(node) + "  " + "%1$-10s".format(node.owner) + "  " + node.name + VirtualShell.CRLF)
    out.flush()
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    arguments.complete(currentFolder, line)
  }
}
