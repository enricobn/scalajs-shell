package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.{VirtualFile, VirtualFolder}

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CdCommand")
class CdCommand extends VirtualCommand {

  private val arguments = new VirtualCommandArguments(
    FolderArgument("folder", false, _.getCurrentUserPermission.execute)
  )

  override def getName: String = "cd"

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*) = {
    val folder: String =
      if (args.isEmpty)
        "/home/" + shell.vum.currentUser
      else
        args(0)

    shell.currentFolder.resolveFolder(folder) match {
      case Left(error) => error.message.ioErrorE
      case Right(fO) => fO match {
        case Some(f) => Right({
          shell.currentFolder = f
          new RunContext()
        })
        case _ => s"cd: $folder: No such file or directory".ioErrorE
      }
    }
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    arguments.complete(currentFolder, line)
  }

}
