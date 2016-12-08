package org.enricobn.shell.impl

import java.util.UUID

import org.enricobn.shell.VirtualCommand
import org.enricobn.terminal.{StringPub, Terminal}
import org.enricobn.vfs._
import org.enricobn.vfs.impl.VirtualUsersManagerImpl
import org.enricobn.vfs.inmemory.InMemoryFS

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

/**
  * Created by enrico on 12/4/16.
  */
@JSExport(name="VirtualShell")
@JSExportAll
class VirtualShell(terminal: Terminal) {
  private val path = new ListBuffer[VirtualFolder]
  val rootPassword = UUID.randomUUID().toString
  val vum = new VirtualUsersManagerImpl(rootPassword)
  val fs = new InMemoryFS(vum)

  private var current: VirtualFolder = fs.root

  val bin = current.mkdir("bin")
  path += bin
  val usr = current.mkdir("usr")
  val usrBin = usr.mkdir("bin")
  path += usrBin
  //    virtualShell.addCommand(bin, new CatCommand)
  createCommandFile(bin, new LsCommand)
  //    virtualShell.addCommand(bin, new TouchCommand)
  //    virtualShell.addCommand(bin, new MkdirCommand)
  //    virtualShell.addCommand(bin, new PwdCommand)
  createCommandFile(bin, new CdCommand)
  current = current.mkdir("home")

  var line = ""

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

  def getCurrentFolder: VirtualFolder = current

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
      file = current.findFileOrThrow(command)
    }
    if (file.isExecutable) {
      file.run(args: _*)
    }
    else {
      throw new VirtualIOException(command + ": Permission denied")
    }
  }
//
//  @throws[VirtualIOException]
//  def setExecutable(name: String) {
//    val file: VirtualFile = current.findFileOrThrow(name)
//    file.setExecutable(true)
//  }

  def setCurrentFolder(folder: VirtualFolder) {
    this.current = folder
  }

  var inputHandler: InputHandler = null

  def start() {
    prompt()
    inputHandler = new InputHandler()
    terminal.onInput(inputHandler)
  }

  private def prompt() {
    terminal.add(current.getPath + "$ ")
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
