package org.enricobn.shell.impl

/**
  * I use it for easier serialization.
  */
case class StringList(value: List[String] = List[String]()) {

  def apply(f : List[String] => List[String]) = StringList(f(value))

}
