package org.enricobn.shell

import java.util.UUID

import org.enricobn.shell.ShellInput.ShellInputDescriptor

/**
  * Created by enrico on 12/13/16.
  */

object ShellInput {

  type ShellInputDescriptor = String

  def newShellInputDescriptor(): ShellInputDescriptor = UUID.randomUUID().toString

}

trait ShellInput {

  def subscribe(fun: Function[String,Unit]) : ShellInputDescriptor

  def close(descriptor: ShellInputDescriptor)

  def closeAll(): Unit

}
