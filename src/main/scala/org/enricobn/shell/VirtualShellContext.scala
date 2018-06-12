package org.enricobn.shell

import org.enricobn.vfs.{Authentication, IOError, VirtualFile, VirtualFolder}

/**
  * Created by enrico on 12/28/16.
  */
trait VirtualShellContext {

  def path : Seq[VirtualFolder]

  def addToPath(folder: VirtualFolder)

  def createCommandFile(folder: VirtualFolder, command: VirtualCommand)(implicit authentication: Authentication) : Either[IOError, VirtualFile]

  def getCommand(file: VirtualFile)(implicit authentication: Authentication): Either[IOError, VirtualCommand]

  def findCommand(command: String, currentFolder: VirtualFolder)(implicit authentication: Authentication) : Option[VirtualFile]

}
