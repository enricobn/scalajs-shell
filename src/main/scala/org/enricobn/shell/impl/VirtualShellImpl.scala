package org.enricobn.shell.impl

import java.util.UUID

import org.enricobn.shell.ShellInput.ShellInputDescriptor
import org.enricobn.shell._
import org.enricobn.shell.impl.RunStatus.Pid
import org.enricobn.terminal.Terminal._
import org.enricobn.terminal.{StringPub, Terminal, TerminalColors, TerminalOperations}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._
import org.scalajs.dom

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

private object RunStatus {

  type Pid = String

  def newPid(): Pid = UUID.randomUUID().toString

}

private case class RunStatus(pid: Pid, process: VirtualProcess, shellInput: ShellInput, background: Boolean) {

  def interactive: Boolean = !background && process.running

}

trait Scheduler {

  def run(callback: Double => Unit): Unit

}

class RequestAnimationFrameScheduler extends Scheduler {

  override def run(callback: Double => Unit): Unit = dom.window.requestAnimationFrame(callback)

}

object UnixLikeVirtualShell {

  def apply(fs: UnixLikeInMemoryFS, terminal: Terminal, currentFolder: VirtualFolder, initialAuthentication: Authentication,
            scheduler: Scheduler = new RequestAnimationFrameScheduler()): VirtualShell = {
    val context = new VirtualShellContextImpl()

    val shell = new VirtualShellImpl(fs, terminal, fs.vum, fs.vsm, context, currentFolder, initialAuthentication, scheduler)

    context.setUserProfile(new VirtualShellUserProfile(shell))

    context.setGlobalProfile(new VirtualShellGlobalProfile(fs, () => shell.authentication))

    shell
  }

}

object VirtualShellImpl {
  private var _shellCount = 0

  private def shellCount: Int = {
    _shellCount += 1
    _shellCount
  }

  private def remove[T](buffer: ListBuffer[T], predicate: T => Boolean): List[T] = {
    val removed = new ListBuffer[T]()

    var loop = true
    while (loop) {
      val indexes = buffer.zipWithIndex
        .filter { case (element, _) => predicate.apply(element) }
        .map { case (_, index) => index }

      if (indexes.isEmpty) {
        loop = false
      } else {
        removed.append(buffer.remove(indexes.head))
      }
    }

    removed.toList
  }

  def prompt(user: String, currentPath: String): String = {
    formatUserPrompt(user) + ":" +
      new TerminalColors().bold().blue(currentPath) + "$ "
  }

  private[impl] def formatUserPrompt(user: String): String = {
    new TerminalColors().bold().green(user).toString()
  }
}

@JSExport(name = "VirtualShell")
@JSExportAll
class VirtualShellImpl(val fs: VirtualFS, val terminal: Terminal, val vum: VirtualUsersManager, val vsm: VirtualSecurityManager, val context: VirtualShellContext,
                       private var _currentFolder: VirtualFolder, private val initialAuthentication: Authentication,
                       private val scheduler: Scheduler = new RequestAnimationFrameScheduler) extends VirtualShell {
  private val name = "Shell " + VirtualShellImpl.shellCount
  private var line = ""
  private val history = new CommandHistory(new CommandHistoryFileStore(this))
  private var x = 0
  private var xPrompt = 0
  private var inputHandler: InputHandler = _
  private val completions = new ShellCompletions(context)
  private val runningCommands = new ListBuffer[RunStatus]()
  private var stopped = true
  private implicit var _authentication: Authentication = initialAuthentication

  private var areInteractiveCommandsRunning = false

  def authentication: Authentication = _authentication

  def currentFolder: VirtualFolder = _currentFolder

  def run(command: String, args: String*): Either[IOError, Unit] =
    run(false, command, args: _*)

  override def runInBackground(command: String, args: String*): Either[IOError, Unit] =
    run(true, command, args: _*)

  def killAll(authentication: Authentication): Either[IOError, Unit] = {
    vum.getUser(authentication) match {
      case Some(user) => if (user == VirtualUsersManager.ROOT) {
        if (areInteractiveCommandsRunning) {
          prompt()
          areInteractiveCommandsRunning = false
        }
        runningCommands.synchronized {
          runningCommands.foreach(_.process.kill())
          runningCommands.clear()
        }
        Right(())
      } else {
        s"Only root can kill all commands.".ioErrorE
      }
      case _ => s"KillAll: cannot find authentication.".ioErrorE
    }
  }


  def currentFolder_=(folder: VirtualFolder) {
    _currentFolder = folder
  }

  def start() {
    prompt()
    startInternal()
  }

  def startWithCommand(background: Boolean, command: String, args: String*): Unit = {
    startInternal()
    run(background, command, args: _*) match {
      case Left(error) =>
        terminal.add(s"Error starting with command $command ${args.mkString(",")}: ${error.message}\n")
        terminal.flush()
        prompt()
      case Right(_) =>
    }
  }

  private def startInternal() {
    inputHandler = new InputHandler()
    terminal.onInput(inputHandler)
    stopped = false
    updateRunningCommands()
  }

  def readLine(onEnter: String => Unit) {
    if (inputHandler != null) {
      terminal.removeOnInput(inputHandler)
    }

    val subscriber: GetStringInputHandler = new GetStringInputHandler({ s =>
      // Don't move it after onEnter: if the onEnter stops the shell, a useless handler is added.
      if (inputHandler != null) {
        terminal.onInput(inputHandler)
      }
      onEnter(s)
    })

    terminal.onInput(subscriber)

  }

  def login(user: String, password: String): Either[IOError, Authentication] =
    vum.logUser(user, password) match {
      case r@Right(authentication) =>
        _authentication = authentication
        r
      case e@Left(_) => e
    }

  def stop(authentication: Authentication): Either[IOError, Unit] = {
    stopped = true
    terminal.removeOnInputs()
    killAll(authentication)
  }

  override def toString: ShellInputDescriptor = name + " (" + super.toString + ")"

  private def run(background: Boolean, command: String, args: String*) = {
    findCommand(command, currentFolder) match {
      case Right(Some(f)) => runFile(background, f, args: _*)
      case Right(None) => s"$command: No such file".ioErrorE
      case Left(error) => Left(error)
    }
  }

  /**
    *
    * @return true if I must show the prompt
    */
  private def runFile(background: Boolean, file: VirtualFile, args: String*): Either[IOError, Unit] = {
    import org.enricobn.vfs.utils.Utils.RightBiasedEither

    if (!vsm.checkExecuteAccess(file)) {
      return "Permission denied!".ioErrorE
    }

    val result = for {
      command <- VirtualCommandOperations.getCommand(file)
      commandInput = new CommandInput()
      process <- command.run(this, commandInput, new CommandOutput(), args: _*)
    } yield {
      (process, commandInput)
    }

    result match {
      case Left(error) => Left(error)
      case Right((process, commandInput)) =>
        if (!background && areInteractiveCommandsRunning) {
          "Interactive command still running. Stop it first.".ioErrorE
        } else {
          val status = RunStatus(RunStatus.newPid(), process, commandInput, background)
          if (!areInteractiveCommandsRunning && !status.interactive) {
            prompt()
          }
          val areInteractiveCommandsRunningBefore = areInteractiveCommandsRunning

          areInteractiveCommandsRunning = areInteractiveCommandsRunning || status.interactive

          if (!areInteractiveCommandsRunningBefore && areInteractiveCommandsRunning && inputHandler != null) {
            terminal.removeOnInput(inputHandler)
          }

          runningCommands.synchronized {
            runningCommands.append(status)
          }
          Right(())
        }
    }

  }

  private def updateRunningCommands(): Unit = {
    if (stopped) {
      return
    }

    runningCommands.synchronized {
      runningCommands.foreach(_.process.update())

      VirtualShellImpl.remove[RunStatus](runningCommands, !_.process.running)
        .foreach(_.shellInput.closeAll())

      if (areInteractiveCommandsRunning && !runningCommands.exists(_.interactive)) {
        areInteractiveCommandsRunning = false

        if (inputHandler != null) {
          terminal.onInput(inputHandler)
        }
        prompt()
      }
    }

    scheduler.run { time: Double =>
      updateRunningCommands()
    }
  }

  private def prompt() {
    val prompt = VirtualShellImpl.prompt(authentication.user, currentFolder.path)

    terminal.add(prompt)
    terminal.flush()
    x = prompt.length
    xPrompt = prompt.length
  }

  private[VirtualShellImpl] class InputHandler extends StringPub#Sub {
    override def notify(pub: mutable.Publisher[String], event: String) {
      if (event == CR) {
        terminal.add(CRLF)
        terminal.flush()
        processLine(line)
        line = ""
      } else if (event == TAB) {
        if (line.nonEmpty) {
          handleCompletion()
        }
      } else if (event == BACKSPACE) {
        if (line.nonEmpty) {
          TerminalOperations.moveLeft(terminal, 1)
          TerminalOperations.eraseFromCursor(terminal)
          terminal.flush()
          line = line.substring(0, line.length - 1)
          x -= 1
        }
      } else if (event.startsWith(ESC)) {
        val cmd = event.substring(1)
        // Up
        if (cmd == "[A") {
          history.prev(line) match {
            case Right(Some(command)) => processHistory(command)
            case Left(error) => showError(error)
            case _ =>
          }
          // Down
        } else if (cmd == "[B") {
          history.succ() match {
            case Right(Some(command)) => processHistory(command)
            case Left(error) => showError(error)
            case _ =>
          }
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

  private def handleCompletion(): Unit = {
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
    TerminalOperations.moveLeft(terminal, x - xPrompt)
    TerminalOperations.eraseFromCursor(terminal)
  }

  private def processLine(line: String) {
    if (line.nonEmpty) {

      history.add(line).left.foreach(showError)

      val words = line.split(" ")
      run(false, words.head, words.tail.toArray: _*) match {
        case Left(error) =>
          terminal.add(error.message + CRLF)
          prompt()
        case Right(_) =>
      }
    } else {
      prompt()
    }
  }

  private[VirtualShellImpl] class CommandInput extends ShellInput {
    private val opened = mutable.HashMap[ShellInputDescriptor, mutable.Subscriber[String, mutable.Publisher[String]]]()

    override def subscribe(fun: Function[String, Unit]): ShellInputDescriptor = {
      val subscriber = new mutable.Subscriber[String, mutable.Publisher[String]] {
        override def notify(pub: mutable.Publisher[String], event: String) {
          fun(event)
        }
      }
      terminal.onInput(subscriber)
      val descriptor = ShellInput.newShellInputDescriptor()
      opened.put(descriptor, subscriber)
      descriptor
    }

    override def close(descriptor: ShellInputDescriptor): Unit = {
      opened.remove(descriptor) match {
        case Some(subscriber) => terminal.removeOnInput(subscriber)
        case _ => // TOD Error
      }
    }

    def closeAll(): Unit = {
      opened.keySet.foreach(close)
    }
  }

  private[VirtualShellImpl] class CommandOutput extends ShellOutput {
    override def write(s: String) {
      terminal.add(s)
    }

    override def flush() {
      terminal.flush()
    }
  }

  private[VirtualShellImpl] class GetStringInputHandler(private val onEnter: String => Unit) extends StringPub#Sub {
    private var line: String = ""

    override def notify(pub: mutable.Publisher[String], event: String) {
      if (event == CR) {
        terminal.removeOnInput(this)

        terminal.add(CRLF)
        terminal.flush()

        onEnter.apply(line)

      } else if (event == BACKSPACE) {
        if (line.nonEmpty) {
          TerminalOperations.moveLeft(terminal, 1)
          TerminalOperations.eraseFromCursor(terminal)
          terminal.flush()
          line = line.substring(0, line.length - 1)
        }
        /*      } else if (event.startsWith(ESC)) {
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
                }*/
      } else {
        terminal.add(event)
        terminal.flush()
        line += event
      }
    }
  }

  private def showError(error: IOError): Unit = {
    dom.window.alert("An error occurred: see javascript console for details.")
  }

}
