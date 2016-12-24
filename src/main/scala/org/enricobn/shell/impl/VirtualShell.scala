package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.terminal.{StringPub, Terminal}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._

import scala.collection.mutable
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/4/16.
  */
object VirtualShell {
  val ESC = 27.toChar.toString
  val TAB = 9.toChar.toString
  val BACKSPACE = 8.toChar.toString
  val CR = 13.toChar.toString
  val LF = 10.toChar.toString
  val CRLF = CR + LF
}
@JSExport(name="VirtualShell")
@JSExportAll
class VirtualShell(terminal: Terminal, val vum: VirtualUsersManager, val context: VirtualShellContext,
                   private var _currentFolder: VirtualFolder) {
  import VirtualShell._
  private var line = ""
  private val history = new CommandHistory
  private var x = 0
  private var xPrompt = 0
  private var inputHandler: InputHandler = null

  def currentFolder = _currentFolder

  def run(command: String, args: String*) : Either[IOError, Unit] = {
    // TODO simplify
    val file = context.path.find(command, currentFolder)
    if (file.isDefined) {
      if (!vum.checkExecuteAccess(file.get)) {
        return "Permission denied!".ioErrorE
      }
      val commandInput = new CommandInput()

      val commandOutput = new CommandOutput()

      terminal.removeOnInputs()

      // TODO get
      val result = file.get.content.right.get.asInstanceOf[VirtualCommand].run(this, commandInput, commandOutput, args: _*)

      terminal.removeOnInputs()
      terminal.onInput(inputHandler)

      result
    } else
      (command + ": No such file").ioErrorE
  }

  def currentFolder_=(folder: VirtualFolder) {
    _currentFolder = folder
  }

  def start() {
    prompt()
    inputHandler = new InputHandler()
    terminal.onInput(inputHandler)
  }

  private def prompt() {
//    val prompt = new ShellColors()
//        .yellow(vum.currentUser + ":")
//        .add(" ")
//        .bold.blue
//          .add(currentFolder.path)
//        .endAll
//        .add("$ ")
    val prompt = vum.currentUser + ":" + currentFolder.path + "$ "

//    val text: String = ESC + "[32m" + vum.currentUser + ESC + "[0m" + ":" + currentFolder.path + "$ "
    terminal.add(prompt.toString)
    terminal.flush()
    x = prompt.length
    xPrompt = prompt.length
  }

  private[VirtualShell] class InputHandler extends StringPub#Sub {
    override def notify(pub: mutable.Publisher[String], event: String) {
      if (event == CR) {
        terminal.add(CRLF)
        terminal.flush()
        processLine(line)
        line = ""
        x = 0
        prompt()
      } else if (event == TAB) {
        if (line.nonEmpty) {
          handleCompletion()
        }
      } else if (event == BACKSPACE) {
        if (line.nonEmpty) {
          moveLeft(1)
          eraseFromCursor()
          terminal.flush()
          line = line.substring(0, line.length -1)
          x -= 1
        }
      } else if (event.startsWith(ESC)) {
        val cmd = event.substring(1)
        // Up
        if (cmd == "[A") {
          history.prev(line).foreach(processHistory)
        // Down
        } else if (cmd == "[B") {
          history.succ().foreach(processHistory)
  //        for (c <- event) {
  //          terminal.add(c.toInt + CRLF)
  //        }
  //        terminal.flush()
        }
      } else {
        terminal.add(event)
        terminal.flush()
        line += event
        x += event.length
      }
    }
  }

  private def handleCompletion() : Unit = {
    context.completions.complete(line, currentFolder) match {
      case NewLine(newLine) =>
        eraseToPrompt()
        line = newLine

        terminal.add(line)
        terminal.flush()
        x = xPrompt + line.length
      case Proposals(proposals) =>
        terminal.add(CRLF)
        proposals.foreach(s => terminal.add(s + CRLF))
        prompt()

        terminal.add(line)
        terminal.flush()
        x = xPrompt + line.length
      case NoProposals() =>
    }
  }

  private def eraseFromCursor() {
    terminal.add(ESC + "[K")
  }

  private def moveLeft(chars: Int) {
    terminal.add(ESC + "[" + chars + "D")
  }

  private def processHistory(command: String) {
    if (x != xPrompt) {
//      terminal.add(ESC + "[" + (x - xPrompt) + "D")
      eraseToPrompt()
    }
    terminal.add(command)
    terminal.flush()
    x = xPrompt + command.length
    line = command
  }

  private def eraseToPrompt(): Unit = {
    moveLeft(x - xPrompt)
    eraseFromCursor()
  }

  private def processLine(line: String) {
    if (line.nonEmpty) {
      history.add(line)
      val words = line.split(" ")
      run(words.head, words.tail.toArray: _*) match {
        case Left(error) => {
          terminal.add(error.message + CRLF)
          terminal.flush()
        }
        case _ =>
      }
    }
  }

  private[VirtualShell] class CommandInput extends ShellInput {
    override def subscribe(fun: Function[String, Unit]) {
      terminal.onInput(new mutable.Subscriber[String, mutable.Publisher[String]] {
        override def notify(pub: mutable.Publisher[String], event: String) {
          fun(event)
        }
      })
    }
  }

  private[VirtualShell] class CommandOutput extends ShellOutput {
    override def write(s: String) {
      terminal.add(s)
    }

    override def flush() {
      terminal.flush()
    }
  }

}
