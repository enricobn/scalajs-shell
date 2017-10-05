import shapeless.ops.hlist.Mapper
import shapeless.{HList, HNil, Poly1}

import scala.language.higherKinds

object TestingShapeless {

  def main(args: Array[String]): Unit = {
    parseArgs(StringArg() :: IntArg() :: HNil)
  }

  def parseArgs[IN <: HList, OUT <: HList, OUT1 <: HList](args: IN)
                                                         (implicit mapper : Mapper.Aux[toArgValue.type, IN, OUT],
                                                          mapper1 : Mapper.Aux[parse.type, OUT, OUT1]) : Unit = {

    val values = List("Hello", "world")

    val argAndValues = args map toArgValue

    val parsed = argAndValues map parse

    println(argAndValues)
  }

}

object toArgValue extends Poly1 {
  implicit val caseString : Case.Aux[StringArg,ArgAndValue[String]] =
    at(arg => ArgAndValue(arg, None))
  implicit val caseInt : Case.Aux[IntArg,ArgAndValue[Int]] =
    at(arg => ArgAndValue(arg, None))
}

object parse extends Poly1 {
  implicit val caseString : Case.Aux[ArgAndValue[String],Option[String]] =
    at(a => toOption(a))
  implicit val caseInt : Case.Aux[ArgAndValue[Int],Option[Int]] =
    at(a => toOption(a))

  private def toOption[T](a: ArgAndValue[T]) =
      a.value match {
        case Some(value) => Some(a.arg.parse(value))
        case _ => None
      }
}

case class ArgAndValue[T](arg: Arg[T], value: Option[String])

sealed trait Arg[T] {

  def parse(s: String) : T

}

final case class StringArg() extends Arg[String] {

  override def parse(s: String): String = s

}

final case class IntArg() extends Arg[Int] {

  override def parse(s: String): Int = s.toInt

}