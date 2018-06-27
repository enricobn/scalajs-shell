package org.enricobn.shell
import org.enricobn.vfs.IOError
import org.enricobn.vfs.utils.Utils

trait VirtualShellProfileRead {

  def keys : Either[IOError, Set[String]]

  def apply(key: String): Either[IOError, Option[String]]

  def getList(key: String, separator : String = ":"): Either[IOError, List[String]] =
    apply(key) match {
      case Right(Some(v)) => Right(v.split(separator).toList)
      case Right(None) => Right(List[String]())
      case Left(error) => Left(error)
    }

  def toMap : Either[IOError, Map[String, Option[String]]] =
    keys match {
      case Right(ks) =>
        val tuples = ks.map(k => (k, apply(k)))
        val value = Utils.liftTuple(tuples)
        value match {
          case Right(ts) => Right(ts.toMap)
          case Left(e) => Left(e)
        }
      case Left(e) => Left(e)
    }

}

trait VirtualShellProfile extends VirtualShellProfileRead {

  def add(key: String, value: String): Either[IOError, Unit]

  def append(key: String, value: String, separator: String = ":"): Either[IOError, Unit] =
    apply(key) match {
      case Right(Some(v)) => add(key, v + separator + value)
      case Right(None) => add(key, value)
      case Left(error) => Left(error)
    }

}

case class VirtualShellProfileMap(map: Map[String, String]) extends VirtualShellProfileRead {

  override def keys: Either[IOError, Set[String]] = Right(map.keys.toSet)

  override def apply(key: String): Either[IOError, Option[String]] = Right(map.get(key))

}
