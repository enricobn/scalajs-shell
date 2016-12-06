package org.enricobn.shell

/**
  * Created by enrico on 12/5/16.
  */
trait KeyHandler {

  def onkeydown(keycode: Int)

  def onkeypress(key: Char)

  def linefeed(keycode: Int) : Boolean = keycode == 13

}
