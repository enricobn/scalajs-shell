package org.enricobn.shell
import org.enricobn.vfs.VirtualFolder

import scala.collection.mutable.ArrayBuffer

/**
  * Created by enrico on 12/14/16.
  */
class ShellPathImpl extends ShellPath {
  private val _path = new ArrayBuffer[VirtualFolder]()

  override def path: Seq[VirtualFolder] = _path

  def add(folder: VirtualFolder): Unit = {
    _path += folder
  }
}
