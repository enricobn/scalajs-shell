package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput, VirtualCommand}
import org.enricobn.terminal.{StringPub, Terminal}
import org.enricobn.vfs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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
class VirtualShell(terminal: Terminal, val vum: VirtualUsersManager, private var _currentFolder: VirtualFolder) {
  import VirtualShell._
  private val path = new ListBuffer[VirtualFolder]
  private var line = ""
  private val history = new CommandHistory
  private var x = 0
  private var xPrompt = 0
  private val commands = new mutable.HashMap[String, VirtualCommand]()

  @throws[VirtualIOException]
  def createCommandFile(folder: VirtualFolder, command: VirtualCommand): VirtualFile = {
    val vfRun = new VirtualFileRun() {
      @scala.throws[VirtualIOException]
      override def run(args: String*): Unit = {
        val shellInput = new ShellInput {
          override def subscribe(fun: Function[String, Unit]) {
            terminal.onInput(new mutable.Subscriber[String, mutable.Publisher[String]] {
              override def notify(pub: mutable.Publisher[String], event: String) {
                fun(event)
              }
            })
          }
        }

        val shellOutput = new ShellOutput {
          override def write(s: String) {
            terminal.add(s)
          }

          override def flush() {
            terminal.flush()
          }
        }

        terminal.removeOnInputs()

        try {
          command.run(VirtualShell.this, shellInput, shellOutput, args: _*)
        } finally {
          terminal.removeOnInputs()
          terminal.onInput(inputHandler)
        }
      }
    }
    val file = folder.createExecutableFile(command.getName, vfRun)
    commands(file.path) = command
    file
  }

  def currentFolder = _currentFolder

  @throws[VirtualIOException]
  def run(command: String, args: String*) {
    val file = fileFromCommand(command)
    if (file.isDefined)
      file.get.run(args: _*)
    else
      throw new VirtualIOException(command + ": No such file")
  }

  private def fileFromCommand(command: String) : Option[VirtualFile] = {
    val first: Option[VirtualFile] = path
      .map(folder => folder.findFile(command))
      .flatMap(_.toList)
      .headOption

    first.orElse(currentFolder.findFile(command))
  }

  def currentFolder_=(folder: VirtualFolder) {
    _currentFolder = folder
  }

  var inputHandler: InputHandler = null

  def start() {
    prompt()
    inputHandler = new InputHandler()
    terminal.onInput(inputHandler)
  }

  def addToPath(folder: VirtualFolder) {
    path += folder
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
        // Down
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
    val words = line.split(" ").toList
    val commandName: String = words.head
    val file = fileFromCommand(commandName)

    val completion: Seq[String] =
      if (file.isDefined) {
        def command = commands(file.get.path)
        if (command != null) {
          command.completion(currentFolder, words.tail.toArray: _*)
        } else {
          Seq[String]()
        }
      } else if (words.length == 1 && !line.endsWith(" ")) {
        path
          .flatMap(_.files)
          .filter(_.getCurrentUserPermission.execute)
          .filter(_.name.startsWith(commandName))
          .map(_.name).toList
      } else {
        Seq[String]()
      }

    if (completion.nonEmpty) {
      if (completion.length == 1) {
        eraseToPrompt
        line = completion.head + " "
      } else {
        terminal.add(CRLF)
        completion.foreach(s => terminal.add(s + CRLF))
        prompt()
      }
      terminal.add(line)
      terminal.flush()
      x = xPrompt + line.length
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
      eraseToPrompt
    }
    terminal.add(command)
    terminal.flush()
    x = xPrompt + command.length
    line = command
  }

  def eraseToPrompt: Unit = {
    moveLeft(x - xPrompt)
    eraseFromCursor()
  }

  private def processLine(line: String) {
    if (line.nonEmpty) {
      history.add(line)
      val words = line.split(" ")
      try {
        run(words.head, words.tail.toArray: _*)
      } catch {
        case ioe: VirtualIOException =>
          terminal.add(ioe.getMessage + CRLF)
          terminal.flush()
      }
    }
  }

}
