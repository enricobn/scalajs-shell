package org.enricobn.shell.impl

import org.enricobn.terminal.Terminal
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

/**
  * Created by enrico on 12/16/16.
  */
class TestShellFactorySpec extends FlatSpec with MockFactory with Matchers {

  "create" should "be fine" in {
    TestShellFactory.create(stub[Terminal])
  }

  "context" should "contain /bin /usr/bin" in {
    val shell = TestShellFactory.create(stub[Terminal])
    assert(shell.context.path(shell.authentication).right.get.map(_.path) == List("/bin", "/usr/bin"))
  }

  "ls" should "be fine" in {
    val terminal = mock[Terminal]
    val shell = TestShellFactory.create(terminal)

    (terminal.add _).expects(where {
      message: String => message.contains(".profile")
    })

    (terminal.add _).expects(where {
      message: String => message.contains("text.txt")
    })

    (terminal.removeOnInputs _).expects().anyNumberOfTimes()

    (terminal.flush _).expects().anyNumberOfTimes()

    (terminal.onInput _).expects(*).anyNumberOfTimes()

    assert(shell.run("ls").isRight)

  }

}
