package org.enricobn.shell.impl

import org.enricobn.shell.{Completion, VirtualCommandOperations, VirtualShellContext}
import org.enricobn.vfs.Authentication
import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/15/16.
  */

sealed trait CompletionResult

final case class NewLine(line: String) extends CompletionResult

final case class Proposals(proposals: Seq[Completion]) extends CompletionResult

final case class NoProposals() extends CompletionResult

class ShellCompletions(context: VirtualShellContext) {

  def complete(line: String, shell: VirtualShell): CompletionResult = {
    implicit val authentication: Authentication = shell.authentication

    val parsedLine = new CommandLine(line)

    if (parsedLine.invalidCommand)
      return NoProposals()

    val proposals =
      if (parsedLine.incompleteCommand) {
        context.path(shell.fs).right.get
          .flatMap(_.files.right.get)
          .filter(_.getCurrentUserPermission.right.exists(_.execute))
          .filter(_.name.startsWith(parsedLine.commandName))
          .map(file => Completion(file.name, file.name)).toList
      } else {
        val fromCommand = for {
          file <- shell.findCommand(parsedLine.commandName, shell.currentFolder).right
          command <- file match {
            case Some(f) => VirtualCommandOperations.getCommand(f).right
            case _ => s"Cannot find command: ${parsedLine.commandName}".ioErrorE.right
          }
        } yield command.completion(line, shell)

        // TODO handle errors? Probably is not necessary to return an Either, if an error occurs during
        // completion, simply print the error to the console and return NoProposals.
        fromCommand.right.toOption.getOrElse(Seq.empty)
      }

    if (proposals.isEmpty) {
      NoProposals()
    } else {
      if (proposals.length == 1) {
        if (parsedLine.incompleteCommand)
          NewLine(proposals.head.absolute + " ")
        else
          NewLine(parsedLine.reconstructLine(proposals.head.absolute))
      } else {
        Proposals(proposals)
      }
    }
  }

}
