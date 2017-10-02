package org.enricobn.shell.impl

import org.enricobn.shell.VirtualShellContext
import org.enricobn.vfs.VirtualFolder

/**
  * Created by enrico on 12/15/16.
  */

sealed trait CompletionResult
final case class NewLine(line: String) extends CompletionResult
final case class Proposals(proposals: Seq[String]) extends CompletionResult
final case class NoProposals() extends CompletionResult

class ShellCompletions(context: VirtualShellContext) {

  def complete(line: String, currentFolder: VirtualFolder): CompletionResult = {
    val parsedLine = new CommandLine(line)

    if (parsedLine.invalidCommand)
      return NoProposals()

    val proposals =
      if (parsedLine.incompleteCommand) {
        context.path
          .flatMap(_.files.right.get)
          .filter(_.getCurrentUserPermission.execute)
          .filter(_.name.startsWith(parsedLine.commandName))
          .map(_.name).toList
      } else {
        val fromCommand = for {
            file <- context.findCommand(parsedLine.commandName, currentFolder)
            command <- context.getCommand(file).right.toOption
          } yield command.completion(line, currentFolder)

        fromCommand.getOrElse(Seq.empty)
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
