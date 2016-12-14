package org.enricobn.shell

import org.enricobn.shell.impl.ParsedLine
import org.enricobn.vfs.{VirtualFile, VirtualFolder}

import scala.collection.mutable

/**
  * Created by enrico on 12/14/16.
  */

sealed trait CompletionResult
case class NewLine(line: String) extends CompletionResult
case class Proposals(proposals: Seq[String]) extends CompletionResult
case class NoProposals() extends CompletionResult

trait Completions {

  def complete(line: String) : CompletionResult

}

class ShellCompletions(path: ShellPath) extends Completions {
  private val commands = new mutable.HashMap[String, VirtualCommand]()

  var currentFolder: VirtualFolder = null

  def addCommandFile(file: VirtualFile, command: VirtualCommand): Unit = {
    commands(file.path) = command
  }

  override def complete(line: String): CompletionResult = {
    val parsedLine = new ParsedLine(line)

    if (parsedLine.invalidCommand)
      return new NoProposals

    val proposals =
      if (parsedLine.incompleteCommand) {
        path.path
          .flatMap(_.files)
          .filter(_.getCurrentUserPermission.execute)
          .filter(_.name.startsWith(parsedLine.commandName))
          .map(_.name).toList
      } else {
        val file = path.find(parsedLine.commandName, currentFolder)
        if (file.isDefined) {
          commands.get(file.get.path)
            .fold(Seq[String]())(cmd => cmd.completion(currentFolder, parsedLine.args.toArray: _*))
        } else {
          Seq[String]()
        }
      }

    if (proposals.isEmpty) {
      new NoProposals
    } else {
      if (proposals.length == 1) {
        if (parsedLine.incompleteCommand)
          new NewLine(proposals.head + " ")
        else
          new NewLine(parsedLine.reconstructLine(proposals.head))
      } else {
        new Proposals(proposals)
      }
    }
  }

}
