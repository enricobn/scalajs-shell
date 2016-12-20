package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.{VirtualFile, VirtualFolder}

import scala.collection.mutable

/**
  * Created by enrico on 12/15/16.
  */
class ShellCompletions(path: ShellPath) extends Completions {
  private val commands = new mutable.HashMap[String, VirtualCommand]()

  def addCommandFile(file: VirtualFile, command: VirtualCommand): Unit = {
    commands(file.path) = command
  }

  override def complete(line: String, currentFolder: VirtualFolder): CompletionResult = {
    val parsedLine = new ParsedLine(line)

    if (parsedLine.invalidCommand)
      return NoProposals()

    val proposals =
      if (parsedLine.incompleteCommand) {
        path.path
          .flatMap(_.files.right.get)
          .filter(_.getCurrentUserPermission.execute)
          .filter(_.name.startsWith(parsedLine.commandName))
          .map(_.name).toList
      } else {
        val file = path.find(parsedLine.commandName, currentFolder)
        if (file.isDefined) {
          commands.get(file.get.path)
            .fold(Seq[String]())(cmd => cmd.completion(line, currentFolder))
        } else {
          Seq[String]()
        }
      }

    if (proposals.isEmpty) {
      NoProposals()
    } else {
      if (proposals.length == 1) {
        if (parsedLine.incompleteCommand)
          NewLine(proposals.head + " ")
        else
          NewLine(parsedLine.reconstructLine(proposals.head))
      } else {
        Proposals(proposals)
      }
    }
  }

}
