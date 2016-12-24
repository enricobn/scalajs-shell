package org.enricobn.shell.impl

import org.enricobn.shell.{ShellInput, ShellOutput}

import scala.collection.mutable

/**
  * Created by enrico on 12/13/16.
  */
object ShellPipe {
  private[ShellPipe] class StringPublisher extends mutable.Publisher[String] {
    override type Pub = mutable.Publisher[String]

    override def publish(event: String) {
      super.publish(event)
    }
  }
}

class ShellPipe extends ShellInput with ShellOutput {
  import ShellPipe._
  val stringPublisher = new StringPublisher

  def subscribe(fun: Function[String,Unit]) {
    stringPublisher.subscribe(new StringPublisher#Sub {
      override def notify(pub: mutable.Publisher[String], event: String) {
        fun(event)
      }
    })
  }

  def write(s: String) {
    stringPublisher.publish(s)
  }

  override def flush() {

  }
}
