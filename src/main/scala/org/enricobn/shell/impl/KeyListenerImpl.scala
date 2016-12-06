package org.enricobn.shell.impl

import org.enricobn.shell.KeyListener
import org.scalajs.dom

/**
  * Created by enrico on 12/5/16.
  */
class KeyListenerImpl extends KeyListener {
  private var keyHandler: org.enricobn.shell.KeyHandler = null
  /*
   * onkeydown is called before onkeypress, and is called even on non char keys (ALT, CTRL ESC etc.).
   * So I check for non char keys onkeydown and char keys onkeypress
   */
  dom.window.onkeydown = { (e: dom.KeyboardEvent) =>
    e.stopPropagation()
    //      dom.console.log("charcode(" + e.charCode + ")")
    //      dom.console.log("key(" + e.key + ")")
    //      dom.console.log("keyCode(" + e.keyCode + ")")
    if (e.keyCode == 13) {
      e.preventDefault()
      if (keyHandler != null) {
        keyHandler.onkeydown(e.keyCode)
      }
    }
  }

  dom.window.onkeypress = { (e: dom.KeyboardEvent) =>
    e.preventDefault()
    e.stopPropagation()
    val char = e.key(0)
    if (keyHandler != null) {
      keyHandler.onkeypress(char)
    }
  }

  override def setHandler(keyHandler: org.enricobn.shell.KeyHandler) {
    this.keyHandler = keyHandler
  }

}
