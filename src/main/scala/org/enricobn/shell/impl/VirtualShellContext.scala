package org.enricobn.shell.impl

import org.enricobn.shell.{ShellPathImpl, VirtualCommand}
import org.enricobn.vfs._

/**
  * Created by enrico on 12/23/16.
  */
class VirtualShellContext {
  val path = new ShellPathImpl()
  val completions = new ShellCompletions(path)

  def addToPath(folder: VirtualFolder) {
    path.add(folder)
  }

  def createCommandFile(folder: VirtualFolder, command: VirtualCommand): Either[IOError, VirtualFile] = {
    for {
      file <- folder.touch(command.getName).right
      _ <- file.setExecutable().right
      _ <- (file.content = command).right
    } yield {
      completions.addCommandFile(file, command)
      file
    }
  }
}
