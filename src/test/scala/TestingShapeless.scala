import shapeless.PolyDefns.~>
import shapeless.ops.hlist.{Mapper, ToList}
import shapeless.{HList, HNil, Poly1}

import scala.language.higherKinds

object TestingShapeless {

  def main(args: Array[String]): Unit = {
    val sets = Set(1) :: Set(1.1) :: HNil

    val opts: HList = sets map choose

    println(opts)

    parseArgs(StringArg() :: IntArg() :: HNil)

  }

  def parseArgs1 : Unit = {
    val args = StringArg() :: IntArg() :: HNil

    val argAndValues = args map toArgValue

    //    implicit val e = implicitly[Mapper[parse.type, mapper.Out]]

    val parsed = argAndValues map parse

    println(argAndValues)

  }

  def parseArgs[IN <: HList, OUT <: HList, OUT1 <: HList](args: IN)
                                                         (implicit mapper : Mapper.Aux[toArgValue.type, IN, OUT],
                                                          mapper1 : Mapper.Aux[parse.type, OUT, OUT1]) : Unit = {

    val values = List("Hello", "world")

//    var index = 0

//    trait toArgValue extends Poly1 {
//      private var index = 0
//      implicit def default[Arg[T]] = at[Arg[_]](arg => {
//        val value = if (values.length <= index) {
//          None
//        } else {
//          Some(values(index))
//        }
//        index += 1
//        ArgAndValue(arg, value)
//
//      })
//    }


//    val argAndValues = ArgAndValueString(StringArg(), Some("Hello")) :: ArgAndValueInt(IntArg(), Some("1")) :: HNil

    val argAndValues = args map toArgValue

    val parsed = argAndValues map parse

    println(argAndValues)


  }

  def mapParse[IN <: HList, OUT <: HList](args: IN)(implicit mapper : Mapper.Aux[parse.type, IN, OUT]) : OUT = {
    args map parse
  }

  def mapArgAndValue[IN <: HList, OUT <: HList](args: IN)(implicit mapper : Mapper.Aux[toArgValue.type, IN, OUT]) : OUT = {
    args map toArgValue
  }

  //  private def mapArgAndValue[T <: HList](args : T)(implicit mapper : Mapper.Aux[toArgValue.type, T]) = {
//    args map toArgValue
//  }


}

class TestingShapeless[H <: HList](args: H)(implicit ktl: ToList[H, Set[_]]) {

  def print(): Unit = {
    println(args.toList)
  }

}

object choose extends (Set ~> Option) {
  def apply[T](s: Set[T]) = s.headOption
}

object toArgValue extends Poly1 {
  implicit val caseString : Case.Aux[StringArg,ArgAndValueString] =
    at(arg => ArgAndValueString(arg, None))
  implicit val caseInt : Case.Aux[IntArg,ArgAndValueInt] =
    at(arg => ArgAndValueInt(arg, None))

//  implicit def all[T] : Case.Aux[Arg[T],ArgAndValue[T]] =
//    at(arg => ArgAndValue(arg, None))
}


//object toArgValue extends (Arg ~> ArgAndValue) {
//
//  def apply[T](f: Arg[T]) = {
//    //      var index = 0 // TODO
//    //      val value = if (values.length <= index) {
//    //        None
//    //      } else {
//    //        Some(values(index))
//    //      }
//    //      index += 1
//    ArgAndValue(f, None)
//  }
//}


//object parse extends (ArgAndValue ~> Option) {
//  def apply[T](a: ArgAndValue[T]) =
//    a.value match {
//      case Some(value) => Some(a.arg.parse(value))
//      case _ => None
//    }
//}

object parse extends Poly1 {
  implicit val caseString : Case.Aux[ArgAndValueString,Option[String]] =
    at(a => toOption(a))
  implicit val caseInt : Case.Aux[ArgAndValueInt,Option[Int]] =
    at(a => toOption(a))

  private def toOption[T](a: ArgAndValue[T]) =
      a.value match {
        case Some(value) => Some(a.arg.parse(value))
        case _ => None
      }
}

sealed trait ArgAndValue[T] {
  val arg: Arg[T]
  val value: Option[String]
}

final case class ArgAndValueString(arg: Arg[String], value: Option[String]) extends ArgAndValue[String]
final case class ArgAndValueInt(arg: Arg[Int], value: Option[String]) extends ArgAndValue[Int]

sealed trait Arg[T] {

  def parse(s: String) : T

}

final case class StringArg() extends Arg[String] {

  override def parse(s: String): String = s

}

final case class IntArg() extends Arg[Int] {

  override def parse(s: String): Int = s.toInt

}