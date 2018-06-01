package org.enricobn.shell.impl

import org.enricobn.shell.{CompletionPath, PartialPath, UnknownPath}
import org.enricobn.vfs.{VirtualFS, VirtualFile, VirtualFolder}

import scala.collection.mutable.ListBuffer

/**
  * Created by enrico on 12/22/16.
  */
trait VirtualCommandArgument[+T] {
  val required: Boolean
  val name: String
  def parse(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Either[String, T]
  def complete(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Seq[String]
}

// TODO the filter is applied only to the parent folder and the result folders.
// For example fod folders:
// /usr/bin/dummy
// /usr/bin/dummy1
//
// if the user types "cd /usr/bin" then tab
// the filter will be applied to /usr/bin, /usr/bin/dummy and /usr/bin/dummy1, but not to /usr.
// I think it depends on the command, so it's not very useful to handle the filter here.
case class FolderArgument(override val name: String, override val required: Boolean,
                          filter: VirtualFolder => Boolean = _ => true)
  extends VirtualCommandArgument[VirtualFolder] {

  override def parse(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Either[String, VirtualFolder] = {
    shell.findFolder(value) match {
      case Left(error) => Left(error.message)
      case Right(folder) => folder match {
        case Some(f) =>
          if (filter.apply(f))
            Right(f)
          else
            Left(s"$name: $value: no such directory")
        case _ => Left(s"$name: $value: no such directory")
      }
    }
  }

  override def complete(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Seq[String] =
      CompletionPath(shell, value) match {
        case UnknownPath() => Seq.empty
        case partialPath: PartialPath =>

          if (!filter.apply(partialPath.folder)) {
            return Seq.empty
          }

          partialPath.folder.folders match {
            case Left(_) => Seq.empty
            case Right(allFolders) =>
              var folders = allFolders.filter(filter)
              if (partialPath.remaining.isDefined) {
                folders = folders.filter(_.name.startsWith(partialPath.remaining.get))
              }
              folders
                .map (partialPath.relativePath + _.name + "/")
                .toSeq
          }
      }

}

case class FileArgument(override val name: String, override val required: Boolean,
                        filter: VirtualFile => Boolean = _ => true)
  extends VirtualCommandArgument[VirtualFile] {

  override def parse(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Either[String, VirtualFile] = {
    shell.findFile(value) match {
      case Left(error) => Left(error.message)
      case Right(Some(file)) =>
        if (filter.apply(file))
          Right(file)
        else
          Left(s"$name: $value: no such file")
      case _ => Left(s"$name: $value: no such file")
    }
  }

  override def complete(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Seq[String] =
    CompletionPath(shell, value) match {
      case UnknownPath() => Seq.empty
      case PartialPath(folder, relativePath, remaining) =>
        val files =
          folder.files match {
            case Left(_) => Seq.empty
            case Right(fx) =>
              fx.filter(filter)
          }

        val folders =
          folder.folders match {
            case Left(_) => Seq.empty
            case Right(f) => f
          }

        val all = (files.map(_.name) ++ folders.map(_.name + VirtualFS.pathSeparator)).toSeq

        (if (remaining.isDefined)
          all.filter(_.startsWith(remaining.get))
        else
          all
          ).map(relativePath + _)

      case _ => Seq.empty
    }

}

case class IntArgument(override val name: String, override val required: Boolean) extends VirtualCommandArgument[Int] {

  override def parse(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Either[String, Int] =
    try {
      Right(value.toInt)
    } catch  {
      case _ : NumberFormatException => Left(s"$name: invalid number.")
    }

  override def complete(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Seq[String] = Seq.empty

}

case class StringArgument(override val name: String, override val required: Boolean) extends VirtualCommandArgument[String] {
  override def parse(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Either[String, String] =
    Right(value)

  override def complete(shell: VirtualShell, value: String, previousArguments: Seq[Any]): Seq[String] = Seq.empty

}

class VirtualCommandArguments(args: VirtualCommandArgument[_]*) {

  private var optional = false
  args.foreach(arg => {
    if (arg.required) {
      if (optional) {
        throw new IllegalArgumentException("Optional arguments before required ones.")
      }
    } else {
      optional = true
    }
  })

  def parse(shell: VirtualShell, commandName: String, commandArgs: String*) : Either[String, Seq[Any]] = {
    if (commandArgs.length < args.count(_.required)) {
      Left("usage: " + commandName + " " + args.foldLeft("")(_ + _.name + " "))
    } else {
      parseInternal(shell, commandArgs.toSeq)
    }
  }

  def complete(shell: VirtualShell, line: String) : Seq[String] = {
    val parsedLine = new CommandLine(line)

    val proposals =
      if (args.isEmpty) {
        Seq.empty
      } else if (parsedLine.args.isEmpty) {
        args.head.complete(shell, "", List.empty)
      } else if (parsedLine.incompleteArgument) {
        parseInternal(shell, parsedLine.argsButLast) match {
          case Left(_) => Seq.empty
          case Right(result) =>
            args(parsedLine.args.length - 1)
                          .complete(shell, parsedLine.lastArgument.get, result)
        }
      } else {
        parseInternal(shell, parsedLine.args) match {
          case Left(_) => Seq.empty
          case Right(parsedArguments) => args(parsedLine.args.length).complete(shell, "", parsedArguments)
        }
      }

    proposals.sorted

    // TODO add a space if it's a precise match.
    // For example if the argument is a file then it's an existent file, in that case and if there are more
    // mandatory arguments, then I add a space to the result, so the user can digit the next arg.

//    if (proposals.size == 1) {
//      proposals.map(_ + " ")
//    } else {
//      proposals
//    }
  }

  private def parseInternal(shell: VirtualShell, lineArgs: Seq[String]) : Either[String, Seq[Any]] = {
    val result = new ListBuffer[Any]
    val zipped = lineArgs
      .zip(args)

    for (pair <- zipped) {
      pair._2.parse(shell, pair._1, result) match {
        case Left(message) => return Left(message)
        case Right(value) => result += value
      }
    }
    Right(result)
  }

}