package org.enricobn.shell.impl

/**
  * Created by enrico on 12/14/16.
  */
class ParsedLine(line: String) {
  private val words = line.split(" ").toList

  def incompleteCommand = words.length == 1 && !line.endsWith(" ")

  def commandName = words.head

  def args = words.tail

  def invalidCommand = line.trim.isEmpty

  def lastArgument = args.lastOption

  def argsButLast =
    if (args.length <= 1)
      List.empty
    else
      args.drop(args.length -1)

  def reconstructLine(lastArgument: String) =
    commandName + " " + argsButLast.fold("")(_ + _ + " ") + lastArgument

}
