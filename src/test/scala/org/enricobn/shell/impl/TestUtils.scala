package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.scalamock.scalatest.MockFactory

object TestUtils extends MockFactory {

  def expectPrompt(terminal : Terminal,  prompt : Boolean = true): Unit = {
    if (prompt) {
      (terminal.add _).expects(where { message: String => message.contains(VirtualShellImpl.formatUserPrompt("guest")) })
    } else {
      (terminal.add _).expects(where { message: String => message.contains(VirtualShellImpl.formatUserPrompt("guest")) }).never()
    }
  }

}
