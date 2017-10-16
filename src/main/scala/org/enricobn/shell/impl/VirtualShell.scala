package org.enricobn.shell.impl

import org.enricobn.shell._
import org.enricobn.terminal.{StringPub, Terminal}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._
import org.scalajs.dom

import scala.collection.mutable
import scala.scalajs.js.annotation.{JSExport, JSExportAll}
import scala.scalajs.js.timers._

/**
  * Created by enrico on 12/4/16.
  */
object VirtualShell {
  private val INTERACTIVE_INTERVAL: Int = 500

  val ESC: String = 27.toChar.toString
  val TAB: String = 9.toChar.toString
  val BACKSPACE: String = 8.toChar.toString
  val CR: String = 13.toChar.toString
  val LF: String = 10.toChar.toString
  val CRLF: String = CR + LF
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
  private var inputHandler: InputHandler = _
  private val completions = new ShellCompletions(context)
  private var runningInteractiveCommands = false
  private var whenDone: () => Boolean = _

  def currentFolder: VirtualFolder = _currentFolder

  def homeFolder: Either[IOError, Option[VirtualFolder]] = currentFolder.resolveFolder(s"/home/${vum.currentUser}")

  def run(command: String, args: String*) : Either[IOError, Boolean] = {
    // TODO simplify
    context.findCommand(command, currentFolder)
      .toRight(new IOError(command + ": No such file")).right
      .flatMap(runFile(_, args: _*))
  }

  /**
    *
    * @param whenDone will be called when interactive commands have been stopped. Return true if you like to
    *                 show the prompt.
    */
  def stopInteractiveCommands(whenDone: () => Boolean): Unit = {
    this.whenDone = whenDone
    runningInteractiveCommands = false
  }

  /**
    *
    * @return true if I must show the prompt
    */
  private def runFile(file: VirtualFile, args: String*) : Either[IOError, Boolean] = {
    if (runningInteractiveCommands) {
      return "Interactive command still running. Stop it first.".ioErrorE
    }

    if (!vum.checkExecuteAccess(file)) {
      return "Permission denied!".ioErrorE
    }

    val result = for {
        command <- context.getCommand(file).right
        _ <- Right(terminal.removeOnInputs()).right
        run <- command.run(this, new CommandInput(), new CommandOutput(), args: _*).right
      } yield {
        run
      }

    result match {
      case Left(error) =>
        terminal.removeOnInputs()
        terminal.onInput(inputHandler)
        error.message.ioErrorE
      case Right(runContext) =>
        Right(
          if (runContext.interactive) {
            setTimeout(INTERACTIVE_INTERVAL) {
              runningInteractiveCommands = true
              updateRunContext(runContext)
            }
            false
          } else {
            terminal.removeOnInputs()
            terminal.onInput(inputHandler)
            true
          }
        )
    }
  }

  private def updateRunContext(runContext: RunContext): Unit = {
    if (!runningInteractiveCommands || !runContext.running) {
      terminal.removeOnInputs()
      terminal.onInput(inputHandler)
      if (!runningInteractiveCommands) {
        if (whenDone.apply()) {
          prompt()
        }
      } else {
        prompt()
      }
      runningInteractiveCommands = false
    } else {
      dom.window.requestAnimationFrame((time: Double) => {
        runContext.update()
        setTimeout(INTERACTIVE_INTERVAL) {
          updateRunContext(runContext)
        }
      })
    }
  }

  def currentFolder_=(folder: VirtualFolder) {
    _currentFolder = folder
  }

  def start() {
    prompt()
    inputHandler = new InputHandler()
    terminal.onInput(inputHandler)
  }

  def stop(): Unit = {
    terminal.removeOnInputs()
  }

  def startWithCommand(command: String, args: String*): Unit = {
    inputHandler = new InputHandler()
    terminal.onInput(inputHandler)
    run(command, args: _*) match {
      case Left(error) =>
        terminal.add(s"Error starting with command $command ${args.mkString(",")}\n")
        terminal.flush()
        prompt()
      case Right(makePrompt) =>
        if (makePrompt) {
          prompt()
        }
    }
  }

  /**
    * @return Right(Some(folder)) if the folder exists, Right(None) if the folder or the path do not exist,
    *         Left(error) if an error occurred.
    */
  def findFolder(path: String): Either[IOError,Option[VirtualFolder]] = {
    val virtualPath = VirtualPath(path)
    val first: Either[IOError, Option[VirtualFolder]] =
      virtualPath.fragments.head match {
        case RootFragment() => Right(Some(currentFolder.root))
        case _ => Right(Some(currentFolder))
      }

    val fragmentsToProcess =
      virtualPath.fragments.head match {
        case RootFragment() => virtualPath.fragments.tail
        case _ => virtualPath.fragments
      }

    fragmentsToProcess.foldLeft(first)((actualFolderE, fragment) =>
      actualFolderE match {
        case Right(Some(actualFolder)) =>
          fragment match {
            case SelfFragment() => Right(Some(actualFolder))
            case ParentFragment() => Right(actualFolder.parent)
            case simple: SimpleFragment => actualFolder.findFolder(simple.name)
            case _ => Left(IOError(s"Invalid path: '$path'"))
          }
        case n@Right(None) => n
        case error => error
      })
  }

  /**
    * @return a Left(error) if the folder or the path does not exist; if you want to check that, use
    * [[VirtualShell.findFolder]] instead.
    */
  def toFolder(path: String): Either[IOError,VirtualFolder] =
    findFolder(path) match {
      case Right(Some(folder)) => Right(folder)
      case Right(None) => Left(IOError(s"Cannot resolve path '$path' from '$currentFolder'."))
      case Left(error) => Left(error)
    }
  /**
    * @return a Left(error) if the file or the path does not exist; if you want to check that, use
    * [[VirtualShell.findFile]] instead.
    */
  def toFile(path: String): Either[IOError,VirtualFile] =
    findFile(path) match {
      case Right(Some(file)) => Right(file)
      case Right(None) => Left(IOError(s"Cannot resolve file '$path' from '$currentFolder'"))
      case Left(error) => Left(error)
    }

  /**
    * @return Right(Some(file)) if the file exists, Right(None) if the file or the path do not exist,
    *         Left(error) if an error occurred.
    */
  def findFile(path: String): Either[IOError,Option[VirtualFile]] = {
    val virtualPath = VirtualPath(path)
    if (virtualPath.parentFragments.isEmpty)
      currentFolder.findFile(virtualPath.name)
    else
      findFolder(virtualPath.parentFragments.get.path) match {
        case Right(Some(folder)) => folder.findFile(virtualPath.name)
        case Right(None) => Right(None)
        case Left(error) => Left(error)
      }
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
        if (processLine(line)) {
          prompt()
        }
        line = ""
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
    completions.complete(line, this) match {
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

  private def processLine(line: String) : Boolean = {
    if (line.nonEmpty) {
      history.add(line)
      val words = line.split(" ")
      run(words.head, words.tail.toArray: _*) match {
        case Left(error) =>
          terminal.add(error.message + CRLF)
          terminal.flush()
          true
        case Right(prompt) => prompt
      }
    } else {
      true
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
