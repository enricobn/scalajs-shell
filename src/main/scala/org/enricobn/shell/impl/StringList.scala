package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal

/**
  * I use it for easier serialization.
  */
case class StringList(value: List[String] = List[String]()) {

  def apply(f : List[String] => List[String]) = StringList(f(value))

  override def toString: String =
    value.mkString(Terminal.LF)

}
