package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.scalamock.matchers.Matchers
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec

class EditLineSpec extends AnyFlatSpec with MockFactory with Matchers {

  "backspace" should "delete from current cursor position" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)

    editLine.add("lp")
    editLine.left()
    editLine.backspace()

    assert(editLine.currentLine == "p")
  }

  "backspace from home" should "not delete" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)

    editLine.add("lp")
    editLine.home()
    editLine.backspace()

    assert(editLine.currentLine == "lp")
  }

  "replaceLine" should "work" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)

    editLine.add("l")
    editLine.add("s")
    editLine.eraseToPrompt()
    editLine.replaceLine("cat . history")

    assert(editLine.x == "cat . history".length)

    editLine.eraseToPrompt()
    editLine.replaceLine("ls")

    assert(editLine.x == 2)
  }

  "canc" should "work" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)

    editLine.add("ls")

    editLine.left()
    editLine.canc()

    assert(editLine.x == 1)
    assert(editLine.currentLine == "l")
  }

  "canc top" should "work" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)

    editLine.add("ls")

    editLine.left()
    editLine.left()
    editLine.canc()

    assert(editLine.x == 0)
    assert(editLine.currentLine == "s")
  }

}
