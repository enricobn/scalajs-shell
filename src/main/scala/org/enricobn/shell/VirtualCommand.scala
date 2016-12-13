package org.enricobn.shell

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.VirtualIOException

/**
  * Created by enrico on 12/4/16.
  */
trait VirtualCommand {
  def getName: String

  @throws[VirtualIOException]
  def run(shell: VirtualShell, shellInput: ShellInput, shellOutput: ShellOutput, args: String*)
}
