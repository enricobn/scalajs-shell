package org.enricobn.shell.impl

import org.enricobn.terminal.{Terminal, TerminalOperations}

class EditLine(private val terminal: Terminal) {
  private var line = ""
  private[impl] var x = 0

  def replaceLine(newLine: String): Unit = {
    eraseToPrompt()
    line = newLine

    terminal.add(line)
    terminal.flush()
    x = line.length
  }

  def add(event: String): Unit = {
    val cursor = x
    line = line.substring(0, cursor) + event + line.substring(cursor)

    val newX = x + event.length

    eraseToPrompt()

    x = newX

    terminal.add(line)

    TerminalOperations.moveCursorLeft(terminal, line.length - x)

    terminal.flush()
  }

  def home(): Unit = {
    TerminalOperations.moveCursorLeft(terminal, x)
    terminal.flush()
    x = 0
  }

  def end(): Unit = {
    TerminalOperations.moveCursorRight(terminal, line.length - x)
    terminal.flush()
    x = line.length
  }

  def left(): Unit = {
    if (x <= 0) {
      return
    }
    TerminalOperations.moveCursorLeft(terminal, 1)
    terminal.flush()
    x += -1
  }

  def right(): Unit = {
    if (x >= line.length) {
      return
    }
    TerminalOperations.moveCursorRight(terminal, 1)
    terminal.flush()
    x += 1
  }

  def backspace(): Unit = {
    if (line.nonEmpty && x > 0) {
      TerminalOperations.moveCursorLeft(terminal, 1)
      TerminalOperations.eraseFromCursor(terminal)
      terminal.add(line.substring(x))
      TerminalOperations.moveCursorLeft(terminal, line.length - x)
      terminal.flush()
      line = line.substring(0, x - 1) + line.substring(x)
      x -= 1
    }
  }

  def reset(): Unit = {
    line = ""
    x = 0
  }

  def currentLine: String = line

  def eraseToPrompt(): Unit = {
    //if (x > 0) {
    TerminalOperations.moveCursorLeft(terminal, x)
    TerminalOperations.eraseFromCursor(terminal)
    x = 0
    //}
  }

  def canc(): Unit = {
    if (line.nonEmpty && x < line.length) {
      TerminalOperations.eraseFromCursor(terminal)
      terminal.add(line.substring(x + 1))
      TerminalOperations.moveCursorLeft(terminal, line.length - x - 1)
      terminal.flush()
      line = line.substring(0, x) + line.substring(x + 1)
    }
  }


}
