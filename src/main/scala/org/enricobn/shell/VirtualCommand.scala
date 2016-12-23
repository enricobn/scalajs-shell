package org.enricobn.shell

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.{IOError, VFSInput, VFSOutput, VirtualFolder}

/**
  * Created by enrico on 12/4/16.
  */
trait VirtualCommand {
  def getName: String

  def run(shell: VirtualShell, shellInput: VFSInput, shellOutput: VFSOutput, args: String*) : Either[IOError, Unit]

  def completion(line: String, currentFolder: VirtualFolder): Seq[String]
}
