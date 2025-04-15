package org.enricobn.shell.impl

import org.enricobn.shell.*
import org.enricobn.vfs.{Authentication, IOError}

abstract class VirtualCommandAbstract(val name: String, val virtualArguments: VirtualCommandArgument[?]*) extends VirtualCommand {

  private val arguments = new VirtualCommandArguments(virtualArguments*)

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*): Either[IOError, VirtualProcess] = {
    arguments.parse(shell, name, args*) match {
      case Left(message) => Left(IOError(name + ": " + message))
      case Right(values) =>
        runParsed(shell, shellInput, shellOutput, values)(shell.authentication)
    }
  }

  override def completion(line: String, shell: VirtualShell): Seq[Completion] = {
    arguments.complete(shell, line)
  }

  def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])(implicit authentication: Authentication): Either[IOError, VirtualProcess]

}
