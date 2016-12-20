package org.enricobn.shell

import org.enricobn.vfs.{IOError, VirtualFolder}
import org.enricobn.vfs.IOError._

/**
  * Created by enrico on 12/14/16.
  */

sealed trait CompletionResult
final case class NewLine(line: String) extends CompletionResult
final case class Proposals(proposals: Seq[String]) extends CompletionResult
final case class NoProposals() extends CompletionResult

trait Completions {
  def complete(line: String, currentFolder: VirtualFolder) : CompletionResult
}

case class PartialPath(folder: VirtualFolder, prefix: String, remaining: Option[String]) {
  // TODO implement VirtualNode.toString with path (can be final ?)
  override def toString: String = "(" + folder.path + "," + prefix + "," + remaining + ")"
}

object Completions {
  def resolveFolder(currentFolder: VirtualFolder, prefix: String) : Either[IOError, PartialPath] = {
    val lastSlash = prefix.lastIndexOf('/')
    if (prefix.startsWith("/")) {
      val remaining = prefix.substring(lastSlash + 1)
      if (lastSlash == 0) {
        Right(PartialPath(currentFolder.root, "/", if (remaining.isEmpty) None else Some(remaining)))
      } else {
        val parent = prefix.substring(0, lastSlash)
        currentFolder.resolveFolder(parent) match {
          case Left(error) => error.message.ioErrorE
          case Right(Some(f)) =>
            Right(PartialPath(
              f,
              parent + "/",
              if (remaining.isEmpty) None else Some(remaining)
            ))
          case _ => s"$parent: no such file or directory.".ioErrorE
        }
      }
    } else {
      if (lastSlash == -1) {
        currentFolder.findFolder(prefix, _.getCurrentUserPermission.execute) match {
          case Left(error) => error.message.ioErrorE
          case Right(Some(f)) => Right(PartialPath(f, prefix, None))
          case _ => Right(PartialPath(currentFolder, "", Some(prefix)))
        }
      } else {
        val parent = prefix.substring(0, lastSlash)
        currentFolder.resolveFolder(parent) match {
          case Left(error) => error.message.ioErrorE
          case Right(Some(f)) => Right(PartialPath(
            f,
            parent + "/",
            if (prefix.length == lastSlash - 1) None else Some(prefix.substring(lastSlash + 1))
          ))
          case _ => s"$parent: no such file or directory.".ioErrorE
        }
      }
    }
  }
}


