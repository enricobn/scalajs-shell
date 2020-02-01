package org.enricobn.shell.impl

import org.enricobn.shell.VirtualShellContext
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.IOError._
import org.enricobn.vfs._
import org.enricobn.vfs.utils.Utils.RightBiasedEither

trait VirtualShell {

  val fs: VirtualFS

  val terminal: Terminal

  val vum: VirtualUsersManager

  val vsm: VirtualSecurityManager

  val context: VirtualShellContext

  def authentication: Authentication

  def currentFolder: VirtualFolder

  def homeFolder: Either[IOError, VirtualFolder] =
    VirtualPath.absolute("home", authentication.user).right.flatMap(_.toFolder(fs)(authentication))

  def run(command: String, args: String*) : Either[IOError, Unit]

  def runInBackground(command: String, args: String*) : Either[IOError, Unit]

  def killAll(authentication: Authentication): Either[IOError, Unit]

  def currentFolder_=(folder: VirtualFolder)

  def start()

  def startWithCommand(background: Boolean, command: String, args: String*): Unit

  def readLine(onEnter: String => Unit) : Unit

  def login(user: String, password: String): Either[IOError, Authentication]

  def stop(authentication: Authentication): Either[IOError, Unit]

  def toFolder(path: String): Either[IOError,VirtualFolder] = {
    if (path == "~")
      return homeFolder

    if (path.startsWith("~") && !path.startsWith("~" + VirtualFS.pathSeparator))
      return s"Invalid path $path".ioErrorE

    val (fromFolderE, pathToSearch) =
      if (path.startsWith("~"))
        (homeFolder, path.substring(2))
      else
        (Right(currentFolder), path)

    for {
      fromFolder <- fromFolderE
      virtualPath <- VirtualPath.of(pathToSearch)
      folder <- virtualPath.toFolder(fromFolder)(authentication)
    } yield folder

  }

  def toFile(path: String): Either[IOError,VirtualFile] = {
    if (path.startsWith("~") && !path.startsWith("~" + VirtualFS.pathSeparator))
      return s"Invalid path $path".ioErrorE

    val (fromFolderE, pathToSearch) =
      if (path.startsWith("~"))
        (homeFolder, path.substring(2))
      else
        (Right(currentFolder), path)

    for {
      fromFolder <- fromFolderE
      virtualPath <- VirtualPath.of(pathToSearch)
      folder <- virtualPath.toFile(fromFolder)(authentication)
    } yield folder
  }

  def findCommand(command: String, currentFolder: VirtualFolder)
  : Either[IOError, Option[VirtualFile]] = {
    for {
      p <- context.path(fs)(authentication).right
      inCurrent <- currentFolder.findFile(command)(authentication).right
    } yield p.map(folder => {
      folder.findFile(command)(authentication).right.get
    })
      .flatMap(_.toList)
      .headOption.orElse(inCurrent)
  }

}