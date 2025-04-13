package org.enricobn.shell.impl

import org.enricobn.shell.VirtualShellContext
import org.enricobn.terminal.Terminal
import org.enricobn.vfs.*
import org.enricobn.vfs.IOError.*

trait VirtualShell {

  val fs: VirtualFS

  val terminal: Terminal

  val vum: VirtualUsersManager

  val vsm: VirtualSecurityManager

  val context: VirtualShellContext

  def authentication: Authentication

  def currentFolder: VirtualFolder

  def homeFolder: Either[IOError, VirtualFolder] =
    VirtualPath.absolute("home", authentication.user).flatMap(_.toFolder(fs)(authentication))

  def run(command: String, args: String*) : Either[IOError, Unit]

  def runInBackground(command: String, args: String*) : Either[IOError, Unit]

  def killAll(authentication: Authentication): Either[IOError, Unit]

  def currentFolder_=(folder: VirtualFolder): Unit

  def start(): Unit

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
      p <- context.path(fs)(authentication)
      inCurrent <- currentFolder.findFile(command)(authentication)
    } yield p.map(folder => {
      folder.findFile(command)(authentication).toOption.get
    })
      .flatMap(_.toList)
      .headOption.orElse(inCurrent)
  }

}