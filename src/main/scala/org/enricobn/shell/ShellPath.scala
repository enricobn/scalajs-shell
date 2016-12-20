package org.enricobn.shell

import org.enricobn.vfs.{VirtualFile, VirtualFolder}

/**
  * Created by enrico on 12/14/16.
  */
trait ShellPath {

  def path: Seq[VirtualFolder]

  def find(command: String, currentFolder: VirtualFolder) : Option[VirtualFile] = {
    val first: Option[VirtualFile] = path
      .map(folder => {
        folder.findFile(command).right.get
      })
      .flatMap(_.toList)
      .headOption

    first.orElse(currentFolder.findFile(command).right.get)
  }
}
