package org.enricobn.shell.impl

import org.enricobn.shell.{RunContext, ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.vfs.{Authentication, IOError}

abstract class VirtualCommandAbstract(val name: String, val virtualArguments: VirtualCommandArgument[_]*) extends VirtualCommand {

  private val arguments = new VirtualCommandArguments(virtualArguments: _*)

  override def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*): Either[IOError, RunContext] = {
    arguments.parse(shell, name, args: _*) match {
      case Left(message) => Left(IOError(name + ": " + message))
      case Right(values) =>
        runParsed(shell, shellInput, shellOutput, values)(shell.authentication)
    }
  }

  override def completion(line: String, shell: VirtualShell): Seq[String] = {
    arguments.complete(shell, line)
  }

  def runParsed(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: Seq[Any])(implicit authentication: Authentication): Either[IOError, RunContext]

}
