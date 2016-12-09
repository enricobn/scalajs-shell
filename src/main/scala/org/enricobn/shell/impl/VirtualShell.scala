package org.enricobn.shell.impl

import org.enricobn.shell.VirtualCommand
import org.enricobn.terminal.{StringPub, Terminal}
import org.enricobn.vfs._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/4/16.
  */
@JSExport(name="VirtualShell")
@JSExportAll
class VirtualShell(terminal: Terminal, val vum: VirtualUsersManager, private var _currentFolder: VirtualFolder) {
  private val path = new ListBuffer[VirtualFolder]
  private var line = ""

  @throws[VirtualIOException]
  def createCommandFile(folder: VirtualFolder, command: VirtualCommand): VirtualFile = {
    val vfRun = new VirtualFileRun() {
      @scala.throws[VirtualIOException]
      override def run(args: String*): Unit = {
        command.run(VirtualShell.this, terminal, args: _*)
      }
    }
    folder.createExecutableFile(command.getName, vfRun)
  }

  def currentFolder = _currentFolder

  @throws[VirtualIOException]
  def run(command: String, args: String*) {
    val first: Option[VirtualFile] = path
      .map(folder => folder.findFile(command))
      .flatMap(_.toList)
      .headOption

    var file: VirtualFile = null
    if (first.isDefined) {
      file = first.get
    }
    else {
      file = currentFolder.findFileOrThrow(command)
    }
    if (file.executable) {
      file.run(args: _*)
    }
    else {
      throw new VirtualIOException(command + ": Permission denied")
    }
  }
//
//  @throws[VirtualIOException]
//  def setExecutable(name: String) {
//    val file: VirtualFile = currentFolder.findFileOrThrow(name)
//    file.setExecutable(true)
//  }

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
    terminal.add(vum.currentUser + "@" + currentFolder.path + "$ ")
    terminal.flush()
  }

  private[VirtualShell] class InputHandler extends StringPub#Sub {
    override def notify(pub: mutable.Publisher[String], event: String) {
      if (event == "\n") {
        terminal.add("\n")
        terminal.flush()
        processLine(line)
        line = ""
        prompt()
      } else {
        terminal.add(event)
        terminal.flush()
        line += event
      }
    }
  }

  private def processLine(line: String) {
    if (line.nonEmpty) {
      val words = line.split(" ").toList
      try {
        terminal.removeOnInput()
        run(words.head, words.drop(1).toArray: _*)
      } catch {
        case ioe: VirtualIOException =>
          terminal.add(ioe.getMessage + "\n")
          terminal.flush()
      } finally {
        terminal.removeOnInput()
        terminal.onInput(inputHandler)
      }
    }
  }

}
