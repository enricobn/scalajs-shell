package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class EditLineSpec extends FlatSpec with MockFactory with Matchers {

  "backspace" should "delete from current cursor position" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)

    editLine.prompt(5) // Hello

    editLine.add("lp")
    editLine.left()
    editLine.backspace()

    assert(editLine.getLine == "p")
  }

  "backspace from home" should "not delete" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)

    editLine.prompt(5) // Hello

    editLine.add("lp")
    editLine.home()
    editLine.backspace()

    assert(editLine.getLine == "lp")
  }

  "replaceLine" should "work" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)

    editLine.prompt(5) // Hello
    editLine.add("l")
    editLine.add("s")
    editLine.eraseToPrompt()
    editLine.replaceLine("cat . history")

    assert(editLine.x == 5 + "cat . history".length)

    editLine.eraseToPrompt()
    editLine.replaceLine("ls")

    assert(editLine.x == 7)
  }

  "canc" should "work" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)
    editLine.prompt(5) // Hello

    editLine.add("ls")

    editLine.left()
    editLine.canc()

    assert(editLine.x == 6)
    assert(editLine.getLine == "l")
  }

  "canc top" should "work" in {
    val term = stub[Terminal]

    val editLine = new EditLine(term)
    editLine.prompt(5) // Hello

    editLine.add("ls")

    editLine.left()
    editLine.left()
    editLine.canc()

    assert(editLine.x == 5)
    assert(editLine.getLine == "s")
  }

}
