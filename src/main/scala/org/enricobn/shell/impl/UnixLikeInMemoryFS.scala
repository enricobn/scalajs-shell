package org.enricobn.shell.impl

import org.enricobn.vfs.*
import org.enricobn.vfs.inmemory.InMemoryFS

object UnixLikeInMemoryFS {

  def apply(fs: InMemoryFS, rootPassword: String): Either[IOError, UnixLikeInMemoryFS] = {
    for {
      authentication <- fs.vum.logUser(VirtualUsersManager.ROOT, rootPassword)
      bin <- mkdir(fs.root, "bin")(authentication)
      usr <- mkdir(fs.root, "usr")(authentication)
      var_ <- mkdir(fs.root, "var")(authentication)
      varLog <- mkdir(var_, "log")(authentication)
      usrBin <- mkdir(usr, "bin")(authentication)
      etc <- mkdir(fs.root, "etc")(authentication)
      home <- mkdir(fs.root, "home")(authentication)
      _ <- etc.createFile("profile", StringMap(Map("PATH" -> "/bin:/usr/bin")))(authentication)
    } yield new UnixLikeInMemoryFS(fs, bin, usr, var_, varLog, usrBin, etc, home)
  }

  private def mkdir(parentFolder: VirtualFolder, name: String)(implicit authentication: Authentication) : Either[IOError, VirtualFolder] = {
    parentFolder.findFolder(name) match {
      case Right(Some(folder)) => Right(folder)
      case Right(None) => parentFolder.mkdir(name)
      case Left(error) => Left(error)
    }
  }

}

class UnixLikeInMemoryFS private (private val fs: InMemoryFS,
                                  val bin: VirtualFolder,
                                  val usr: VirtualFolder,
                                  val `var` : VirtualFolder,
                                  val varLog : VirtualFolder,
                                  val usrBin : VirtualFolder,
                                  val etc : VirtualFolder,
                                  val home : VirtualFolder) extends VirtualFS {

  override def root: VirtualFolder = fs.root

  val vum: VirtualUsersManager = fs.vum
  val vsm: VirtualSecurityManager = fs.vsm
  val notifier: VirtualFSNotifier = fs.notifier
}