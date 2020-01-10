package org.enricobn.shell.impl

import org.enricobn.shell.VirtualShellContext
import org.enricobn.terminal.Terminal
import org.enricobn.vfs._

trait VirtualShell {

  val fs: VirtualFS

  val terminal: Terminal

  val vum: VirtualUsersManager

  val vsm: VirtualSecurityManager

  val context: VirtualShellContext

  def authentication: Authentication

  def currentFolder: VirtualFolder

  def homeFolder: Either[IOError, VirtualFolder] = currentFolder.resolveFolderOrError(s"/home/${authentication.user}")(authentication)

  def run(command: String, args: String*) : Either[IOError, Unit]

  def runInBackground(command: String, args: String*) : Either[IOError, Unit]

  def killAll(authentication: Authentication): Either[IOError, Unit]

  def currentFolder_=(folder: VirtualFolder)

  def start()

  def startWithCommand(background: Boolean, command: String, args: String*): Unit

  def readLine(onEnter: String => Unit) : Unit

  def login(user: String, password: String): Either[IOError, Authentication]

  def stop(authentication: Authentication): Either[IOError, Unit]

  /**
    * @return Right(Some(folder)) if the folder exists, Right(None) if the folder or the path do not exist,
    *         Left(error) if an error occurred.
    */
  def findFolder(path: String): Either[IOError,Option[VirtualFolder]] = {
    val virtualPath = VirtualPath(path)

    val resolvedPath = virtualPath.fragments.head match {
      case SimpleFragment("~") => homeFolder.right.map(_.path).right.get + VirtualFS.pathSeparator +
        VirtualPath(virtualPath.fragments.tail).path
      case _ => path
    }

    currentFolder.resolveFolder(resolvedPath)(authentication)
  }

  /**
    * @return a Left(error) if the folder or the path does not exist; if you want to check that, use
    * [[VirtualShell.findFolder]] instead.
    */
  def toFolder(path: String): Either[IOError,VirtualFolder] =
    findFolder(path) match {
      case Right(Some(folder)) => Right(folder)
      case Right(None) => Left(IOError(s"Cannot resolve path '$path' from '$currentFolder'."))
      case Left(error) => Left(error)
    }

  /**
    * @return a Left(error) if the file or the path does not exist; if you want to check that, use
    * [[VirtualShell.findFile]] instead.
    */
  def toFile(path: String): Either[IOError,VirtualFile] =
    findFile(path) match {
      case Right(Some(file)) => Right(file)
      case Right(None) => Left(IOError(s"Cannot resolve file '$path' from '$currentFolder'"))
      case Left(error) => Left(error)
    }

  /**
    * @return Right(Some(file)) if the file exists, Right(None) if the file or the path do not exist,
    *         Left(error) if an error occurred.
    */
  def findFile(path: String): Either[IOError,Option[VirtualFile]] = {
    val virtualPath = VirtualPath(path)
    if (virtualPath.parentFragments.isEmpty)
      currentFolder.findFile(virtualPath.name)(authentication)
    else
      findFolder(virtualPath.parentFragments.get.path) match {
        case Right(Some(folder)) => folder.findFile(virtualPath.name)(authentication)
        case Right(None) => Right(None)
        case Left(error) => Left(error)
      }
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