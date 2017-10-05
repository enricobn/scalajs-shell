import scala.language.higherKinds

import shapeless.nat._
import shapeless.ops.hlist.{At, Mapper}
import shapeless.{HList, HNil, Nat, Poly1}

object TestingShapeless {

  def main(args: Array[String]): Unit = {
    val parsed = parseArgs(StringArg() :: IntArg() :: HNil)

    val string = parsed.get(_0)
    val int = parsed.get(_1)

    println(string)
    println(int)
  }

  // Thanks to https://stackoverflow.com/questions/35535543/getting-elements-from-an-hlist
  class HListContainer[L <: HList](hl: L) {
    def get(n: Nat)(implicit at: At[L, n.N]): at.Out = hl[n.N]
  }

  def parseArgs[IN <: HList, OUT <: HList, OUT1 <: HList](args: IN)
                                                         (implicit mapper : Mapper.Aux[toArgValue.type, IN, OUT],
                                                          mapper1 : Mapper.Aux[parse.type, OUT, OUT1]): HListContainer[OUT1] = {

    val values = List("Hello", "world")

    val argAndValues = args map toArgValue

    new HListContainer(argAndValues map parse)
  }

}

object toArgValue extends Poly1 {
  implicit val caseString : Case.Aux[StringArg,ArgAndValue[String]] =
    at(arg => ArgAndValue(arg, Some("a string")))
  implicit val caseInt : Case.Aux[IntArg,ArgAndValue[Int]] =
    at(arg => ArgAndValue(arg, Some("1")))
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