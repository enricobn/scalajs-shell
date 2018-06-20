package org.enricobn.shell
import org.enricobn.vfs.IOError

trait VirtualShellProfile {

  def add(key: String, value: String): Either[IOError, Unit]

  def apply(key: String): Either[IOError, Option[String]]

  def append(key: String, value: String, separator: String = ":"): Either[IOError, Unit] =
    apply(key) match {
      case Right(Some(v)) => add(key, v + separator + value)
      case Right(None) => add(key, value)
      case Left(error) => Left(error)
    }

  def getList(key: String, separator : String = ":"): Either[IOError, List[String]] =
    apply(key) match {
      case Right(Some(v)) => Right(v.split(separator).toList)
      case Right(None) => Right(List[String]())
      case Left(error) => Left(error)
    }

}
