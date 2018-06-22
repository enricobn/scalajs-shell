package org.enricobn.shell.impl

import org.enricobn.shell.{VirtualCommand, VirtualShellContext, VirtualShellProfile}
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._
import org.enricobn.vfs.utils.Utils

/**
  * Created by enrico on 12/23/16.
  */
class VirtualShellContextImpl(private val fs: VirtualFS) extends VirtualShellContext {
  private var profile : VirtualShellProfile = _

  def setProfile(profile: VirtualShellProfile): Unit =
    this.profile = profile

  override def path(implicit authentication: Authentication): Either[IOError, Seq[VirtualFolder]] =
    profile.getList("PATH") match {
      case Right(l) => Utils.lift(l.map(fs.root.resolveFolder(_)))
          .right.map { l => l.filter(_.isDefined).map(_.get)}
      case Left(error) => Left(error)
    }

  def addToPath(folder: VirtualFolder): Either[IOError, Unit] =
    profile.append("PATH", folder.path)

  // TODO I don't like it here, has nothing to do with context!
  def createCommandFile(folder: VirtualFolder, command: VirtualCommand)(implicit authentication: Authentication): Either[IOError, VirtualFile] = {
    for {
      file <- folder.touch(command.name).right
      _ <- file.setExecutable.toLeft(None).right
      _ <- file.setContent(command).toLeft(None).right
    } yield {
      file
    }
  }

  // TODO I don't like it here, has nothing to do with context!
  def getCommand(file: VirtualFile)(implicit authentication: Authentication): Either[IOError, VirtualCommand] =
    file.getContent match {
      case Left(error) => Left(error)
      case Right(command: VirtualCommand) => Right(command)
      case _ => "File is not a command.".ioErrorE
    }

  // TODO I don't like it here, has something about context, but I think its better to let context hold only
  // the state.
  def findCommand(command: String, currentFolder: VirtualFolder)(implicit authentication: Authentication)
  : Either[IOError, Option[VirtualFile]] = {
    for {
      p <- path.right
      inCurrent <- currentFolder.findFile(command).right
    } yield p.map(folder => {
        folder.findFile(command).right.get
      })
      .flatMap(_.toList)
      .headOption.orElse(inCurrent)
  }

}
