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

  def name: String = "ls"

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*)
  : Either[IOError, RunContext] = {
    val errorOrFolder = arguments.parse(shell, name, args: _*) match {
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

  override def completion(line: String, shell: VirtualShell): Seq[String] = {
    arguments.complete(shell, line)
  }
}
