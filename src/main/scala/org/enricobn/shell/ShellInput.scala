package org.enricobn.shell

/**
  * Created by enrico on 12/13/16.
  */
trait ShellInput {
  def subscribe(fun: Function[String,Unit]) : Unit
}
