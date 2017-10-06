import scala.language.higherKinds
import shapeless.nat._
import shapeless.ops.hlist.{At, Mapper, Reverse, RightFolder}
import shapeless.{HList, HNil, Nat, Poly1, Poly2}
import shapeless._

object TestingShapeless {

  def main(args: Array[String]): Unit = {
    val parsed = parseArgs(StringArg() :: IntArg() :: HNil, List("Hello", "1"))

    val string = parsed.get(_0)
    val int = parsed.get(_1)

    println(string)
    println(int)
  }

  // Thanks to https://stackoverflow.com/questions/35535543/getting-elements-from-an-hlist
  class HListContainer[L <: HList](hl: L) {
    def get(n: Nat)(implicit at: At[L, n.N]): at.Out = hl[n.N]
  }

  def parseArgs[IN <: HList, OUT <: HList, OUT1 <: HList](args: IN, values: List[String])
      (implicit folder: RightFolder.Aux[IN, (HNil, List[String], Int), toArgValue.type, (OUT, List[String], Int)],
      mapper1 : Mapper.Aux[parse.type, OUT, OUT1]): HListContainer[OUT1] = {

    val argAndValues = args.foldRight((HNil : HNil, values.reverse, 0))(toArgValue)._1

    new HListContainer(argAndValues map parse)
  }

}

// thanks to https://stackoverflow.com/questions/26631231/how-to-transform-an-hlist-to-another-hlist-with-foldright-foldleft
object toArgValue extends Poly2 {
  implicit def caseString[B <: HList] : Case.Aux[
    StringArg,
    (B, List[String], Int),
    (ArgAndValue[String] :: B, List[String], Int)
    ] = at[StringArg, (B, List[String], Int)] {
      case (arg, (values, list, index)) => (ArgAndValue(arg, Some(list(index))) :: values, list, index + 1)
  }

  implicit def caseInt[B <: HList] : Case.Aux[
    IntArg,
    (B, List[String], Int),
    (ArgAndValue[Int] :: B, List[String], Int)
    ] = at[IntArg, (B, List[String], Int)] {
    case (arg, (values, list, index)) => (ArgAndValue(arg, Some(list(index))) :: values, list, index + 1)
  }

//  implicit def caseDef[A, B <: HList] : Case.Aux[
//    Arg[A],
//    (B, List[String], Int),
//    (ArgAndValue[A] :: B, List[String], Int)
//    ] = at[Arg[A], (B, List[String], Int)] {
//    case (arg, (values, list, index)) => (ArgAndValue(arg, Some(list(index))) :: values, list, index + 1)
//  }

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