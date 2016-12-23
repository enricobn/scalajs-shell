package org.enricobn.shell.impl

import org.enricobn.vfs.{VirtualFile, VirtualFolder}

import scala.collection.mutable.ListBuffer

/**
  * Created by enrico on 12/22/16.
  */
trait VirtualCommandArgument[+T] {
  val required: Boolean
  val name: String
  def parse(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]) : Either[String, T]
  def complete(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]) : Seq[String]
}

abstract case class FolderArgument(override val name: String, override val required: Boolean) extends VirtualCommandArgument[VirtualFolder] {
  override def parse(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]): Either[String, VirtualFolder] = {
    currentFolder.resolveFolder(value) match {
      case Left(error) => Left(error.message)
      case Right(folder) => folder match {
        case Some(f) => Right(f)
        case _ => Left(s"$name: $value: no such directory")
      }
    }
  }
}

abstract case class FileArgument(override val name: String, override val required: Boolean) extends VirtualCommandArgument[VirtualFile] {
  override def parse(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]): Either[String, VirtualFile] = {
    currentFolder.findFile(value) match {// TODO relativeFile
      case Left(error) => Left(error.message)
      case Right(Some(file)) => Right(file)
      case _ => Left(s"$name: $value: no such file")
    }
  }
}

case class IntArgument(override val name: String, override val required: Boolean) extends VirtualCommandArgument[Int] {
  override def parse(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]): Either[String, Int] =
    try {
      Right(value.toInt)
    } catch  {
      case e: NumberFormatException => Left(s"$name: invalid number.")
    }

  override def complete(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]): Seq[String] =
    Seq.empty
}

case class StringArgument(override val name: String, override val required: Boolean) extends VirtualCommandArgument[String] {
  override def parse(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]): Either[String, String] =
    Right(value)

  override def complete(currentFolder: VirtualFolder, value: String, previousArguments: Seq[Any]): Seq[String] =
    Seq.empty
}

class VirtualCommandArguments(args: List[VirtualCommandArgument[_]]) {
  // TODO check that required arguments are at the end
  // TODO check if commandArgs are more than args
  def parse(currentFolder: VirtualFolder, commandArgs: String*) : Either[String, Seq[Any]] = {
    if (commandArgs.length < args.count(_.required)) {
      Left("usage: " + args.foldLeft("")(_ + _.name + " "))
    } else {
      getResult(currentFolder, commandArgs.toSeq)
    }
  }

  // TODO check if commandArgs are more than args
  def complete(currentFolder: VirtualFolder, line: String) : Seq[String] = {
    val parsedLine = new ParsedLine(line)

    val proposals =
      if (parsedLine.args.isEmpty) {
        args.head.complete(currentFolder, "", List.empty)
      } else if (parsedLine.incompleteArgument) {
        getResult(currentFolder, parsedLine.argsButLast) match {
          case Left(message) => Seq.empty
          case Right(result) =>
            args(parsedLine.args.length - 1).complete(currentFolder, parsedLine.lastArgument.get, result) // TODO get
        }
      } else {
        getResult(currentFolder, parsedLine.args) match {
          case Left(message) => Seq.empty
          case Right(result) => args(parsedLine.args.length).complete(currentFolder, "", result)
        }
      }

    if (proposals.size == 1) {
      proposals.map(_ + " ")
    } else {
      proposals
    }
  }

  private def getResult(currentFolder: VirtualFolder, lineArgs: Seq[String]) : Either[String, Seq[Any]] = {
    val result = new ListBuffer[Any]
    val zipped = lineArgs
      .zip(args)

    for (pair <- zipped) {
      pair._2.parse(currentFolder, pair._1, result) match {
        case Left(message) => return Left(message)
        case Right(value) => result += value
      }
    }
    Right(result)
  }

}