package org.enricobn.shell

import org.enricobn.vfs.utils.Utils
import org.enricobn.vfs.{Authentication, IOError, VirtualFS, VirtualFolder}

/**
  * Created by enrico on 12/28/16.
  */
trait VirtualShellContext {

  def profile : VirtualShellProfileRead

  def path(fs: VirtualFS)(implicit authentication: Authentication): Either[IOError, Seq[VirtualFolder]] =
    profile.getList("PATH") match {
      case Right(l) => Utils.lift(l.map(fs.root.resolveFolder(_)))
        .right.map { l => l.filter(_.isDefined).map(_.get)}
      case Left(error) => Left(error)
    }

}