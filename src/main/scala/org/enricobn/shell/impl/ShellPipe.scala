package org.enricobn.shell.impl

import org.enricobn.shell.ShellInput.ShellInputDescriptor
import org.enricobn.shell.{ShellInput, ShellOutput}
import org.enricobn.vfs.Listeners

/**
  * Created by enrico on 12/13/16.
  */
object ShellPipe {
  private[ShellPipe] class StringPublisher extends Listeners[String] {
  }
}

class ShellPipe extends ShellInput with ShellOutput {
  import ShellPipe.*
  val stringPublisher = new StringPublisher

  def subscribe(fun: Function[String,Unit]) : ShellInputDescriptor = {
    stringPublisher.subscribe(event =>new StringPublisher {
        fun(event)
    })
    ShellInput.newShellInputDescriptor()
  }

  def write(s: String): Unit = {
    stringPublisher.publish(s)
  }

  override def flush(): Unit = {

  }

  // TODO
  override def close(descriptor: ShellInputDescriptor): Unit = {}

  // TODO
  override def closeAll(): Unit = {}
}
