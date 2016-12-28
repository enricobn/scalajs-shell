package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.vfs.VirtualFolder

/**
  * Created by enrico on 12/15/16.
  */
class ShellCompletions(context: VirtualShellContext) extends Completions {

  override def complete(line: String, currentFolder: VirtualFolder): CompletionResult = {
    val parsedLine = new ParsedLine(line)

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
