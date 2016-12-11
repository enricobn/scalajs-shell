package org.enricobn.shell.impl

/**
  * Created by enrico on 12/11/16.
  */
class ShellColors {
  private val sb: StringBuilder = new StringBuilder
  private val stack = new scala.collection.mutable.Stack[String]
  private var _length = 0

  def length = _length

  def m(value: Int): ShellColors = {
    stack.push("\u001B[" + value + "m")
    sb.append(stack.head)
    this
  }

  def m(value: Int, s: String): ShellColors = {
    m(value).add(s).end
  }

  def bold: ShellColors = {
    m(1)
  }

  def bold(s: String): ShellColors = {
    m(1, s)
  }

  def green: ShellColors = {
    m(32)
  }

  def green(s: String): ShellColors = {
    m(32, s)
  }

  def yellow: ShellColors = {
    m(33)
  }

  def yellow(s: String): ShellColors = {
    m(33, s)
  }

  def blue: ShellColors = {
    m(34)
  }

  def blue(s: String): ShellColors = {
    m(34, s)
  }

  def end: ShellColors = {
    stack.pop
    sb.append("\u001B[0m")
    for (s <- stack) {
      sb.append(s)
    }
    this
  }

  def endAll: ShellColors = {
    stack.clear
    sb.append("\u001B[0m")
    this
  }

  def add(s: String): ShellColors = {
    sb.append(s)
    _length += s.length
    this
  }

  override def toString: String = {
    endAll
    sb.toString
  }

}
