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

case class PartialPath(folder: Option[VirtualFolder], remaining: Option[String]) {
  // TODO implement VirtualNode.toString with path (can be final ?)
  override def toString: String = "(" + folder.map(_.path).getOrElse("None") + "," + remaining + ")"
}

object Completions {
  def resolveFolder(currentFolder: VirtualFolder, prefix: String) : PartialPath = {
    println(prefix)
    if (prefix.startsWith("/")) {
      val lastSlash = prefix.lastIndexOf('/')
      if (lastSlash == 0) {
        PartialPath(Some(currentFolder.root), Some(prefix.substring(lastSlash + 1)))
      } else {
        try {
          // TODO currentPermission must be handled by resolveFolder
          PartialPath(Some(currentFolder.resolveFolder(prefix.substring(0, lastSlash))),
            if (prefix.length == lastSlash -1) None else Some(prefix.substring(lastSlash +1)))
        } catch {
          case ioe: VirtualIOException =>
            PartialPath(None, None)
        }
      }
    } else {
      val lastSlash = prefix.lastIndexOf('/')
      if (lastSlash == -1) {
        try {
          val folder: Option[VirtualFolder] = currentFolder.findFolder(prefix, _.getCurrentUserPermission.execute)
          if (folder.isDefined) {
            PartialPath(folder, None)
          } else {
            PartialPath(Some(currentFolder), Some(prefix))
          }
        } catch {
          case ioe: VirtualIOException =>
            PartialPath(None, None)
        }
      } else {
        try {
          PartialPath(Some(currentFolder.resolveFolder(prefix.substring(0, lastSlash))),
            if (prefix.length == lastSlash -1) None else Some(prefix.substring(lastSlash +1)))
        } catch {
          case ioe: VirtualIOException =>
            PartialPath(None, None)
        }
      }
    }
  }
}


