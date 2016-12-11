package org.enricobn.shell.impl

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by enrico on 12/10/16.
  */
class CommandHistory(maxSize: Int = 100) {
  private val history = new ListBuffer[String]
  private var current = 0
  private var currentLine: Option[String] = None

  def prev(currentLine: String) : Option[String] = {
    if (current == history.length) {
      this.currentLine = Some(currentLine)
    }
    val next = current - 1
    if (next >= 0) {
      current = next
      Some(history(next))
    } else {
      None
    }
  }

  def succ() : Option[String] = {
    val next = current + 1
    if (next < history.length) {
      current = next
      Some(history(next))
    } else {
      current = history.length
      currentLine
    }
  }

  def add(command: String) {
    if (!history.lastOption.contains(command)) {
      history += command
      if (history.length > maxSize) {
        history.remove(0)
      }
    }
    current = history.length
    currentLine = None
  }

}
