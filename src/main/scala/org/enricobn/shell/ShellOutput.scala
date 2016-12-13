package org.enricobn.shell

/**
  * Created by enrico on 12/13/16.
  */
trait ShellOutput {

  def write(s: String) : Unit

  def flush() : Unit

}
