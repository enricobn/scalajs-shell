package org.enricobn.shell

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.{IOError, VirtualFile, VirtualFolder}

import IOError._

/**
  * Created by enrico on 12/4/16.
  */
trait VirtualCommand {
  // TODO rename to name
  def getName: String

  def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*) : Either[IOError, RunContext]

  def completion(line: String, currentFolder: VirtualFolder): Seq[String]

  override def toString: String = "[executable file]"

}
