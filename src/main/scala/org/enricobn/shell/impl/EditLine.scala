package org.enricobn.shell.impl

import org.enricobn.terminal.{Terminal, TerminalOperations}

class EditLine(private val terminal: Terminal) {
  private var line = ""
  private[impl] var x = 0
  private var xPrompt = 0

  def replaceLine(newLine: String): Unit = {
    eraseToPrompt()
    line = newLine

    terminal.add(line)
    terminal.flush()
    x = xPrompt + line.length
  }

  def add(event: String): Unit = {
    val cursor = distanceFromPrompt
    line = line.substring(0, cursor) + event + line.substring(cursor)

    val newX = x + event.length

    eraseToPrompt()

    x = newX

    terminal.add(line)
    TerminalOperations.moveCursorLeft(terminal, xPrompt + line.length - x)
    terminal.flush()
  }

  def home(): Unit = {
    TerminalOperations.moveCursorLeft(terminal, distanceFromPrompt)
    terminal.flush()
    x = xPrompt
  }

  def end(): Unit = {
    TerminalOperations.moveCursorRight(terminal, xPrompt + line.length - x)
    terminal.flush()
    x = xPrompt + line.length
  }

  def left(): Unit = {
    if (x <= xPrompt ) {
      return
    }
    TerminalOperations.moveCursorLeft(terminal, 1)
    terminal.flush()
    x += -1
  }

  def right(): Unit = {
    if (x >= xPrompt + line.length) {
      return
    }
    TerminalOperations.moveCursorRight(terminal, 1)
    terminal.flush()
    x += 1
  }

  def backspace(): Unit = {
    if (line.nonEmpty && x > xPrompt) {
      TerminalOperations.moveCursorLeft(terminal, 1)
      TerminalOperations.eraseFromCursor(terminal)
      terminal.add(line.substring(distanceFromPrompt))
      TerminalOperations.moveCursorLeft(terminal, line.length - distanceFromPrompt)
      terminal.flush()
      line = line.substring(0, distanceFromPrompt -1) + line.substring(distanceFromPrompt)
      x -= 1
    }
  }

  private def distanceFromPrompt: Int = {
    x - xPrompt
  }

  def reset(): Unit = {
    line = ""
    x = xPrompt
  }

  def prompt(len: Int): Unit = {
      x = len
      xPrompt = len
  }

  def getLine: String = line

  def eraseToPrompt(): Unit = {
    TerminalOperations.moveCursorLeft(terminal, distanceFromPrompt)
    TerminalOperations.eraseFromCursor(terminal)
    x = xPrompt
  }

  def canc(): Unit = {
    if (line.nonEmpty && x < xPrompt + line.length) {
      TerminalOperations.eraseFromCursor(terminal)
      terminal.add(line.substring(distanceFromPrompt + 1))
      TerminalOperations.moveCursorLeft(terminal, line.length - distanceFromPrompt -1)
      terminal.flush()
      line = line.substring(0, distanceFromPrompt) + line.substring(distanceFromPrompt + 1)
    }
  }


}
