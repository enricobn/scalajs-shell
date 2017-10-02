package org.enricobn.shell.impl

/**
  * Created by enrico on 12/14/16.
  */
class CommandLine(line: String) {
  private val words = line.split(" ").toList

  def incompleteCommand: Boolean = words.length == 1 && !line.endsWith(" ")

  def incompleteArgument: Boolean = words.length > 1 && !line.endsWith(" ")

  def commandName: String = words.head

  def args: List[String] = words.tail

  def invalidCommand: Boolean = line.trim.isEmpty

  def lastArgument: Option[String] = args.lastOption

  def argsButLast: List[String] =
    if (args.length <= 1)
      List.empty
    else
      args.take(args.length -1)

  def reconstructLine(lastArgument: String): String =
    if (incompleteArgument) {
      commandName + " " + argsButLast.fold("")(_ + _ + " ") + lastArgument
    } else {
      commandName + " " + args.fold("")(_ + _ + " ") + lastArgument
    }

}
