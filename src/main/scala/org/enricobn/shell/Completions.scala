package org.enricobn.shell

import org.enricobn.vfs.{IOError, VirtualFolder}
import org.enricobn.vfs.IOError._

import IOError._
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

sealed trait CompletionPath

/**
  * @param folder the resolved folder
  * @param relativePath the path relative to the currentFolder of the resolveFolder method
  * @param remaining the remaining part from the folder
  */
case class PartialPath(folder: VirtualFolder, relativePath: String, remaining: Option[String]) extends CompletionPath {
  // TODO implement VirtualNode.toString with path (can be final ?)
  override def toString: String = "(" + folder.path + "," + relativePath + "," + remaining + ")"
}

/**
  *
  * @param folder the resolved folder
  * @param relativePath the path relative to the currentFolder of the resolveFolder method
  */
case class CompletePath(folder: VirtualFolder, relativePath: String) extends CompletionPath

case class UnknownPath() extends CompletionPath

object Completions {
  def resolveFolder(currentFolder: VirtualFolder, prefix: String) : CompletionPath = {
    val lastSlash = prefix.lastIndexOf('/')

    val tmpResult =
      if (lastSlash < 0) {
        PartialPath(currentFolder, "", Some(prefix))
      } else {
        val remaining =
          if (lastSlash == prefix.length -1) {
            None
          } else {
            Some(prefix.substring(lastSlash + 1))
          }

        if (lastSlash == 0) {
          PartialPath(currentFolder.root, "/", remaining)
        } else {
          val parent = prefix.substring(0, lastSlash)
          currentFolder.resolveFolder(parent) match {
            case Left(error) => new UnknownPath
            case Right(Some(folder)) =>
              PartialPath(folder, prefix.substring(0, lastSlash) + "/", remaining)
            case _ => new UnknownPath
          }
        }
      }

    // I try to resolve the remaining part
    tmpResult match {
      case PartialPath(folder, relativePath, Some(remaining)) =>
        folder.findFolder(remaining, _ => true) match {
          case Left(error) => UnknownPath()
          case Right(Some(f)) => CompletePath(f, remaining)
          case _ => PartialPath(folder, relativePath, Some(remaining))
        }
      case x => x
    }
  }
}


