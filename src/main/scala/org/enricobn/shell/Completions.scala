package org.enricobn.shell

import org.enricobn.vfs.{VirtualFolder, VirtualIOException}

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
  def resolveFolder(currentFolder: VirtualFolder, prefix: String) : Option[PartialPath] = {
    try {
      val lastSlash = prefix.lastIndexOf('/')
      if (prefix.startsWith("/")) {
        val remaining = prefix.substring(lastSlash + 1)
        if (lastSlash == 0) {
          Some(PartialPath(currentFolder.root, "/", if (remaining.isEmpty) None else Some(remaining)))
        } else {
          val parent = prefix.substring(0, lastSlash)
          Some(
            PartialPath(
              currentFolder.resolveFolder(parent),
              parent + "/",
              if (remaining.isEmpty) None else Some(remaining)
            )
          )
        }
      } else {
        if (lastSlash == -1) {
          val folder = currentFolder.findFolder(prefix, _.getCurrentUserPermission.execute)
          if (folder.isDefined) {
            Some(PartialPath(folder.get, prefix, None))
          } else {
            Some(PartialPath(currentFolder, "", Some(prefix)))
          }
        } else {
          val parent = prefix.substring(0, lastSlash)
          Some(
            PartialPath(
              currentFolder.resolveFolder(parent),
              parent + "/",
              if (prefix.length == lastSlash -1) None else Some(prefix.substring(lastSlash +1))
            )
          )
        }
      }
    } catch {
      case ioe: VirtualIOException =>
        None
    }
  }
}


