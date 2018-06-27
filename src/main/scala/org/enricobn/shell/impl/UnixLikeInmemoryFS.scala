package org.enricobn.shell.impl

import org.enricobn.vfs._
import org.enricobn.vfs.inmemory.InMemoryFS

object UnixLikeInMemoryFS {

  def apply(rootPassword: String) = {
    val fs = new InMemoryFS(rootPassword)

    for {
      authentication <- fs.vum.logUser(VirtualUsersManager.ROOT, rootPassword).right
      bin <- mkdir(fs.root, "bin")(authentication).right
      usr <- mkdir(fs.root, "usr")(authentication).right
      var_ <- mkdir(fs.root, "var")(authentication).right
      varLog <- mkdir(var_, "log")(authentication).right
      usrBin <- mkdir(usr, "bin")(authentication).right
      etc <- mkdir(fs.root, "etc")(authentication).right
      home <- mkdir(fs.root, "home")(authentication).right
      _ <- etc.createFile("profile", StringMap(Map("PATH" -> "/bin:/usr/bin")))(authentication).right
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

}