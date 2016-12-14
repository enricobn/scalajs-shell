package org.enricobn.shell

import org.enricobn.vfs.{VirtualFile, VirtualFolder}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by enrico on 12/14/16.
  */

sealed trait CompletionResult
case class NewLine(line: String) extends CompletionResult
case class Proposals(proposals: Seq[String]) extends CompletionResult
case class NoCompletion() extends CompletionResult

trait Completions {

  def complete(line: String) : CompletionResult

}

class ShellCompletions(path: ShellPath, currentFolder: VirtualFolder) extends Completions {
  private val commands = new mutable.HashMap[String, VirtualCommand]()

  def addCommandFile(file: VirtualFile, command: VirtualCommand): Unit = {
    commands(file.path) = command
  }

  override def complete(line: String): CompletionResult = {
    val words = line.split(" ").toList
    val commandName: String = words.head
    val file = path.find(commandName, currentFolder)

    val completion: Seq[String] =
      if (file.isDefined) {
        def command = commands(file.get.path)
        if (command != null) {
          command.completion(currentFolder, words.tail.toArray: _*)
        } else {
          Seq[String]()
        }
      } else if (words.length == 1 && !line.endsWith(" ")) {
        path.path
          .flatMap(_.files)
          .filter(_.getCurrentUserPermission.execute)
          .filter(_.name.startsWith(commandName))
          .map(_.name).toList
      } else {
        Seq[String]()
      }

    if (completion.nonEmpty) {
      if (completion.length == 1) {
        new NewLine(completion.head + " ")
      } else {
        new Proposals(completion)
      }
    } else {
      new NoCompletion
    }

  }
}
