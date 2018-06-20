package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal

case class StringMap(value : Map[String, String] = Map[String, String]()) {

  def apply(f : Map[String, String] => Map[String, String]) = StringMap(f(value))

  override def toString: String =
    value.map { case (k, v) => s"$k=$v"}.mkString(Terminal.LF)

}
