package org.enricobn.shell

import org.enricobn.vfs.*
import org.enricobn.vfs.utils.Utils

/**
  * Created by enrico on 12/28/16.
  */
trait VirtualShellContext {

  def profile : VirtualShellProfileRead

  def path(fs: VirtualFS)(implicit authentication: Authentication): Either[IOError, Seq[VirtualFolder]] =
    profile.getList("PATH") match {
      case Right(l) =>
        Utils.lift(l.map(VirtualPath.of(_).flatMap(_.toFolder(fs.root))))
      case Left(error) => Left(error)
    }

}