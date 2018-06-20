package org.enricobn.shell

import org.enricobn.vfs.{Authentication, IOError, VirtualFile, VirtualFolder}

/**
  * Created by enrico on 12/28/16.
  */
trait VirtualShellContext {

  def setProfile(profile: VirtualShellProfile)

  def path(implicit authentication: Authentication) : Either[IOError, Seq[VirtualFolder]]

  def addToPath(folder: VirtualFolder) : Either[IOError, Unit]

  def createCommandFile(folder: VirtualFolder, command: VirtualCommand)(implicit authentication: Authentication) : Either[IOError, VirtualFile]

  def getCommand(file: VirtualFile)(implicit authentication: Authentication): Either[IOError, VirtualCommand]

  def findCommand(command: String, currentFolder: VirtualFolder)(implicit authentication: Authentication)
  : Either[IOError, Option[VirtualFile]]

}
