package org.enricobn.shell.impl

import org.enricobn.shell.VirtualShellProfileMap
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
// to access members of structural types (new {}) without warnings
import scala.language.reflectiveCalls

class CascadeShellProfileSpec extends FlatSpec with MockFactory with Matchers {

  def fixture(profiles: Map[String, String]*) = CascadeShellProfile(profiles.map(VirtualShellProfileMap))

  "PATH" should "be resolved from parent" in {
    val m1 = Map("PATH" -> "/bin:/usr/bin" )
    val m2 = Map("PATH" -> "$PATH:/home/enrico")
    val f = fixture(m1, m2)

    assert(f("PATH") == Right(Some("/bin:/usr/bin:/home/enrico")))
  }

  "PATH" should "not be resolved from parent" in {
    val m1 = Map("PATH1" -> "/bin:/usr/bin" )
    val m2 = Map("PATH" -> "$PATH:/home/enrico")
    val f = fixture(m1, m2)

    assert(f("PATH") == Right(Some(":/home/enrico")))
  }

  "PATH" should "be resolved from parents" in {
    val m1 = Map("PATH" -> "/bin" )
    val m2 = Map("PATH" -> "$PATH:/usr/bin" )
    val m3 = Map("PATH" -> "$PATH:/home/enrico")
    val f = fixture(m1, m2, m3)

    assert(f("PATH") == Right(Some("/bin:/usr/bin:/home/enrico")))
  }

  "vars" should "be resolved from all parents" in {
    val m1 = Map("PATH" -> "/bin", "USER" -> "enrico")
    val m2 = Map("PATH" -> "$PATH:/usr/bin" )
    val m3 = Map("PATH" -> "$PATH:/home/enrico", "USR" -> "$USER")
    val f = fixture(m1, m2, m3)

    assert(f("PATH") == Right(Some("/bin:/usr/bin:/home/enrico")))
    assert(f("USR") == Right(Some("enrico")))
  }

  "key" should "be the of all profiles" in {
    val m1 = Map("PATH" -> "/bin")
    val m2 = Map("USER" -> "enrico")
    val m3 = Map("PWD" -> "/home/enrico")
    val f = fixture(m1, m2, m3)

    assert(f.keys == Right(Set("PATH", "USER", "PWD")))
  }

  "reference to a variable in the same profile" should "be resolved" in {
    val m1 = Map("USER" -> "enrico", "PATH" -> "/bin:/home/$USER")
    val f = fixture(m1)

    assert(f("PATH") == Right(Some("/bin:/home/enrico")))
  }

  "self reference to a variable in the same profile" should "not be resolved" in {
    val m1 = Map("PATH" -> "$PATH:/bin")
    val f = fixture(m1)

    assert(f("PATH") == Right(Some(":/bin")))
  }

}
