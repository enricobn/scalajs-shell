package org.enricobn.shell

import org.enricobn.vfs.{IOError, VirtualFile, VirtualFolder}

/**
  * Created by enrico on 12/28/16.
  */
trait VirtualShellContext {

  def path : Seq[VirtualFolder]

  def addToPath(folder: VirtualFolder)

  def createCommandFile(folder: VirtualFolder, command: VirtualCommand): Either[IOError, VirtualFile]

  def getCommand(file: VirtualFile): Either[IOError, VirtualCommand]

  def findCommand(command: String, currentFolder: VirtualFolder) : Option[VirtualFile]

}
