package org.enricobn.shell.impl

import org.enricobn.shell.{Completions, ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.vfs.{VirtualFolder, VirtualIOException}

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CdCommand")
class CdCommand extends VirtualCommand {
  override def getName: String = "cd"

  @throws[VirtualIOException]
  override def run(shell: VirtualShell, in: ShellInput, out: ShellOutput, args: String*) {
    val folder: String =
      if (args.isEmpty)
        "/home/" + shell.vum.currentUser
      else
        args(0)

    try {
      val currentFolder = shell.currentFolder.resolveFolder(folder)
      shell.currentFolder = currentFolder
    }
    catch {
      case e: VirtualIOException =>
        throw new VirtualIOException("cd: " + e.getMessage, e)
    }
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    val parsedLine = new ParsedLine(line)
    val start = parsedLine.lastArgument.getOrElse("")

    if (parsedLine.lastArgument.isDefined) {
      val partialPath = Completions.resolveFolder(currentFolder, parsedLine.lastArgument.get)
      println(partialPath)
      if (partialPath.folder.isDefined) {
        if (partialPath.remaining.isDefined) {
          partialPath.folder.get.folders
            .filter(_.getCurrentUserPermission.execute)
            .filter(_.name.startsWith(partialPath.remaining.get))
            .map(_.path + "/") // TODO if currentFolder I want the name
            .toSeq
        } else {
          partialPath.folder.get.folders
            .filter(_.getCurrentUserPermission.execute)
            .map(_.path)
            .toSeq
        }
      } else {
        Seq.empty
      }
    } else {
      currentFolder.folders
        .filter(_.getCurrentUserPermission.execute)
        .map(_.name)
        .filter(_.startsWith(start))
        .toSeq
    }
  }

}
