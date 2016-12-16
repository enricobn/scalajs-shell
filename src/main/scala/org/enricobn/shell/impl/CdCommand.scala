package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualCommand}
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
      val partialPath = resolveFolder(currentFolder, parsedLine.lastArgument.get)
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

  case class PartialPath(folder: Option[VirtualFolder], remaining: Option[String]) {
    // TODO implement VirtualNode.toString with path (can be final ?)
    override def toString: String = "(" + folder.map(_.path).getOrElse("None") + "," + remaining + ")"
  }

  private def resolveFolder(currentFolder: VirtualFolder, prefix: String) : PartialPath = {
    println(prefix)
    if (prefix.startsWith("/")) {
      val lastSlash = prefix.lastIndexOf('/')
      if (lastSlash == 0) {
        PartialPath(Some(currentFolder.root), Some(prefix.substring(lastSlash + 1)))
      } else {
        try {
          // TODO currentPermission must be handled by resolveFolder
          PartialPath(Some(currentFolder.resolveFolder(prefix.substring(0, lastSlash))),
            if (prefix.length == lastSlash -1) None else Some(prefix.substring(lastSlash +1)))
        } catch {
          case ioe: VirtualIOException =>
            PartialPath(None, None)
        }
      }
    } else {
      val lastSlash = prefix.lastIndexOf('/')
      if (lastSlash == -1) {
        try {
          val folder: Option[VirtualFolder] = currentFolder.findFolder(prefix, _.getCurrentUserPermission.execute)
          if (folder.isDefined) {
            PartialPath(folder, None)
          } else {
            PartialPath(Some(currentFolder), Some(prefix))
          }
        } catch {
          case ioe: VirtualIOException =>
            PartialPath(None, None)
        }
      } else {
        try {
          PartialPath(Some(currentFolder.resolveFolder(prefix.substring(0, lastSlash))),
            if (prefix.length == lastSlash -1) None else Some(prefix.substring(lastSlash +1)))
        } catch {
          case ioe: VirtualIOException =>
            PartialPath(None, None)
        }
      }
    }
  }
}
