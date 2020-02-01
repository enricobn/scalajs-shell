package org.enricobn.shell

import org.enricobn.shell.impl.VirtualShell
import org.enricobn.vfs.{Authentication, VirtualFS, VirtualFolder}
/**
  * Created by enrico on 12/14/16.
  */
object CompletionPath {

  def apply(shell: VirtualShell, prefix: String): CompletionPath = {
    implicit val authentication: Authentication = shell.authentication

    val lastSlash = prefix.lastIndexOf('/')

    val tmpResult =
      if (lastSlash < 0) {
        PartialPath(shell.currentFolder, "", Some(prefix))
      } else {
        val remaining =
          if (lastSlash == prefix.length -1) {
            None
          } else {
            Some(prefix.substring(lastSlash + 1))
          }

        if (lastSlash == 0) {
          PartialPath(shell.currentFolder.root, "/", remaining)
        } else {
          val parent = prefix.substring(0, lastSlash)
          shell.toFolder(parent) match {
            case Left(_) => new UnknownPath
            case Right(folder) =>
              PartialPath(folder, prefix.substring(0, lastSlash) + VirtualFS.pathSeparator, remaining)
          }
        }
      }

    // I try to resolve the remaining part
    tmpResult match {
      case PartialPath(folder, relativePath, Some(remaining)) =>
        folder.findFolder(remaining) match {
          case Left(_) => UnknownPath()
          case Right(Some(f)) => PartialPath(f, remaining + VirtualFS.pathSeparator, None)
          case _ => PartialPath(folder, relativePath, Some(remaining))
        }
      case x => x
    }
  }
}

sealed trait CompletionPath

/**
  * @param folder the resolved folder
  * @param relativePath the path relative to the currentFolder of the resolveFolder method
  * @param remaining the remaining part from the folder
  */
case class PartialPath(folder: VirtualFolder, relativePath: String, remaining: Option[String]) extends CompletionPath {
  override def toString: String = "(" + folder + "," + relativePath + "," + remaining + ")"
}

///**
//  *
//  * @param folder the resolved folder
//  * @param relativePath the path relative to the currentFolder of the resolveFolder method
//  */
//case class CompletePath(folder: VirtualFolder, relativePath: String) extends CompletionPath

case class UnknownPath() extends CompletionPath

