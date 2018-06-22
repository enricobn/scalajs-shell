package org.enricobn.shell

import org.enricobn.vfs.utils.Utils
import org.enricobn.vfs.{Authentication, IOError, VirtualFile, VirtualFolder}

/**
  * Created by enrico on 12/28/16.
  */
trait VirtualShellContext {

  def path(implicit authentication: Authentication) : Either[IOError, Seq[VirtualFolder]]

  def addToPath(folder: VirtualFolder) : Either[IOError, Unit]

  def createCommandFile(folder: VirtualFolder, command: VirtualCommand)(implicit authentication: Authentication) : Either[IOError, VirtualFile]

  def getCommand(file: VirtualFile)(implicit authentication: Authentication): Either[IOError, VirtualCommand]

  def findCommand(command: String, currentFolder: VirtualFolder)(implicit authentication: Authentication)
  : Either[IOError, Option[VirtualFile]]

  def createCommandFiles(folder: VirtualFolder, commands: VirtualCommand*)(implicit rootAuthentication: Authentication): Either[IOError, List[VirtualFile]] =
    Utils.lift(commands.map(createCommandFile(folder, _)))

}
