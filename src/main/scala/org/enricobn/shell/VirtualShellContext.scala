package org.enricobn.shell

import org.enricobn.vfs.{Authentication, IOError, VirtualFolder}

/**
  * Created by enrico on 12/28/16.
  */
trait VirtualShellContext {

  def path(implicit authentication: Authentication) : Either[IOError, Seq[VirtualFolder]]

  def addToPath(folder: VirtualFolder) : Either[IOError, Unit]

}
