package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.IOError._
import org.enricobn.vfs.VirtualFolder

import scala.scalajs.js.annotation.JSExport

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CdCommand")
class CdCommand extends VirtualCommand {
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
        case Some(f) => Right(shell.currentFolder = f)
        case _ => s"cd: $folder: No such file or directory".ioErrorE
      }
    }
  }

  override def completion(line: String, currentFolder: VirtualFolder): Seq[String] = {
    val parsedLine = new ParsedLine(line)
    val start = parsedLine.lastArgument.getOrElse("")

    if (parsedLine.lastArgument.isDefined) {
      Completions.resolveFolder(currentFolder, parsedLine.lastArgument.get) match {
        case UnknownPath() => Seq.empty
        case partialPath: PartialPath =>
          partialPath.folder.folders match {
            case Left(error) => Seq.empty
            case Right(folders) =>
              if (partialPath.remaining.isDefined) {
                folders
                  .filter (_.getCurrentUserPermission.execute)
                  .filter (_.name.startsWith (partialPath.remaining.get) )
                  .map (partialPath.relativePath + _.name + "/")
                  .toSeq
              } else {
                folders
                  .filter (_.getCurrentUserPermission.execute)
                  .map (partialPath.relativePath + _.name + "/")
                  .toSeq
              }
          }
      }
    } else {
      currentFolder.folders match {
        case Left(error) => Seq.empty
        case Right(fs) =>
          fs
            .filter(_.getCurrentUserPermission.execute)
            .map(_.name + "/")
            .filter(_.startsWith(start))
            .toSeq
      }
    }
  }

}
