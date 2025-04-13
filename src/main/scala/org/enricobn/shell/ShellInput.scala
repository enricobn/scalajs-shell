package org.enricobn.shell

import org.enricobn.shell.ShellInput.ShellInputDescriptor

import scala.util.Random.nextInt

/**
  * Created by enrico on 12/13/16.
  */

object ShellInput {

  type ShellInputDescriptor = String

  def newShellInputDescriptor(): ShellInputDescriptor = nextInt().toString

}

trait ShellInput {

  def subscribe(fun: Function[String,Unit]) : ShellInputDescriptor

  def close(descriptor: ShellInputDescriptor): Unit

  def closeAll(): Unit

}
