package org.enricobn.shell.impl

import java.util.regex.Pattern

import org.enricobn.shell.VirtualShellProfileRead
import org.enricobn.vfs.IOError
import org.enricobn.vfs.utils.Utils

object CascadeShellProfile {
  private val PATTERN = "\\$([A-Za-z0-9]+)".r.pattern
}

case class CascadeShellProfile(profiles: Seq[VirtualShellProfileRead]) extends VirtualShellProfileRead {

  import CascadeShellProfile._

  override def apply(key: String): Either[IOError, Option[String]] = {
    if (profiles.isEmpty) {
      return Right(None)
    }

    val parentProfile = CascadeShellProfile(profiles.dropRight(1))

    val profile = profiles.last

    profile(key) match {
      case Right(Some(v)) =>
        val matcher = PATTERN.matcher(v)
        var text = v

        while ( {
          matcher.find
        }) {
          val variable = matcher.group(1)

          val resolvedValue =
            if (variable == key) {
              parentProfile(variable)
            } else {
              apply(variable) match {
                case varValue@Right(Some(_)) => varValue
                case _ => parentProfile(variable)
              }
            }

          val fromParent = resolvedValue match {
            case Right(Some(pv)) => pv.replace("\\", "\\\\")
            case Right(None) => ""
            case Left(error) =>
              // TODO ERROR
              println(s"Error resolving variable '$variable': ${error.message}")
              ""
          }
          val subExpr = Pattern.compile(Pattern.quote(matcher.group(0)))
          text = subExpr.matcher(text).replaceAll(fromParent)
        }
        Right(Some(text))
      case Right(None) => parentProfile(key)
      case other@_ => other
    }
  }

  override def keys: Either[IOError, Set[String]] = {
    val seq = profiles.map(_.keys)
    Utils.lift(seq) match {
      case Right(l) => Right(l.flatMap(_.toSet).toSet)
      case Left(e) => Left(e)
    }
  }

}
