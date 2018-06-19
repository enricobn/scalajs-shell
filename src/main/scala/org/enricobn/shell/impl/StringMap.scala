package org.enricobn.shell.impl

case class StringMap(value : Map[String, String] = Map[String, String]()) {

  def apply(f : Map[String, String] => Map[String, String]) = StringMap(f(value))

}
