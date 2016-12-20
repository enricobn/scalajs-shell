package org.enricobn.shell.impl

import org.enricobn.shell.{Completions, ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.vfs.{VirtualFolder}

import scala.scalajs.js.annotation.JSExport

import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/5/16.
  */
@JSExport(name = "CdCommand")
class CdCommand extends VirtualCommand {
  override def getName: String = "cd"

  override def run(shell: VirtualShell, in: ShellInput, out: ShellOutput, args: String*) = {
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
        case Left(error) => Seq.empty // TODO error
        case Right(partialPath) => {
          partialPath.folder.folders match {
            case Left(error) => Seq.empty // TODO error
            case Right(f) =>
              if (partialPath.remaining.isDefined) {
                f
                  .filter (_.getCurrentUserPermission.execute)
                  .filter (_.name.startsWith (partialPath.remaining.get) )
                  .map (partialPath.prefix + _.name + "/")
                  .toSeq
              } else {
                f
                  .filter (_.getCurrentUserPermission.execute)
                  .map (partialPath.prefix + _.name + "/")
                  .toSeq
              }
          }
        }
      }
    } else {
      currentFolder.folders match {
        case Left(error) => Seq.empty // TODO error
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
