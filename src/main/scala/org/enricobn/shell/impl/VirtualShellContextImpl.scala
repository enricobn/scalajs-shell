package org.enricobn.shell.impl

import org.enricobn.shell.{VirtualShellContext, VirtualShellProfile}
import org.enricobn.vfs._
import org.enricobn.vfs.utils.Utils

/**
  * Created by enrico on 12/23/16.
  */
class VirtualShellContextImpl(private val fs: VirtualFS) extends VirtualShellContext {
  private var globalProfile : VirtualShellProfile = _
  private var userProfile : VirtualShellProfile = _

  def setGlobalProfile(profile: VirtualShellProfile): Unit = {
    globalProfile = profile
  }

  def setUserProfile(profile: VirtualShellProfile): Unit =
    userProfile = profile

  override def path(implicit authentication: Authentication): Either[IOError, Seq[VirtualFolder]] =
    userProfile.getList("PATH") match {
      case Right(l) => Utils.lift(l.map(fs.root.resolveFolder(_)))
          .right.map { l => l.filter(_.isDefined).map(_.get)}
      case Left(error) => Left(error)
    }

  def addToPath(folder: VirtualFolder): Either[IOError, Unit] =
    userProfile.append("PATH", folder.path)

}
