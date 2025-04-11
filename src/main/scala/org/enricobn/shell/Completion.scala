package org.enricobn.shell

case class Completion(absolute: String, relative: String) extends Ordered[Completion] {
  override def compare(that: Completion): Int = absolute.compareTo(that.absolute)
}
