package org.enricobn.shell

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.{VirtualFolder}

import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/4/16.
  */
trait VirtualCommand {
  def getName: String

  def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*) : IOEff[Unit]

  def completion(line: String, currentFolder: VirtualFolder): Seq[String]
}
