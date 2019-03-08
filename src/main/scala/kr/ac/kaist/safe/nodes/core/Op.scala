/**
 * *****************************************************************************
 * Copyright (c) 2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.nodes.core

import scala.util.parsing.combinator._

// CORE Operators
abstract class Op {
  val keyword: String
  override def toString: String = keyword
}

// Unary Operators
abstract class UOp(val keyword: String) extends Op {
  def apply(value: Value): Value = (this, value) match {
    case (ONeg, Num(n)) => Num(-n)
    case (OBNot, LNum(n)) => LNum(~n)
    case (_, _) => error(s"wrong type of value for the operator $this: $value")
  }
}
case object ONeg extends UOp("-")
case object OBNot extends UOp("!")

// Binary Operators
abstract class BOp(val keyword: String) extends Op {
  def apply(left: Value, right: Value): Value = (this, left, right) match {
    // double operations
    case (OPlus, Num(l), Num(r)) => Num(l + r)
    case (OSub, Num(l), Num(r)) => Num(l - r)
    case (OMul, Num(l), Num(r)) => Num(l * r)
    case (ODiv, Num(l), Num(r)) => Num(l / r)
    case (OMod, Num(l), Num(r)) => Num(l % r)
    case (OLt, Num(l), Num(r)) => Bool(l < r)

    // string operations
    case (OPlus, Str(l), Str(r)) => Str(l + r)
    case (OLt, Str(l), Str(r)) => Bool(l < r)

    // long operations
    case (OPlus, LNum(l), LNum(r)) => LNum(l + r)
    case (OSub, LNum(l), LNum(r)) => LNum(l - r)
    case (OMul, LNum(l), LNum(r)) => LNum(l * r)
    case (ODiv, LNum(l), LNum(r)) => LNum(l / r)
    case (OMod, LNum(l), LNum(r)) => LNum(l % r)
    case (OLt, LNum(l), LNum(r)) => Bool(l < r)
    case (OBAnd, LNum(l), LNum(r)) => LNum(l & r)
    case (OBOr, LNum(l), LNum(r)) => LNum(l | r)
    case (OBXOr, LNum(l), LNum(r)) => LNum(l ^ r)
    case (OLShift, LNum(l), LNum(r)) => LNum(l << r)
    case (OSRShift, LNum(l), LNum(r)) => LNum(l >> r)
    case (OURShift, LNum(l), LNum(r)) => LNum(l >>> r)
    case _ => error(s"wrong type: $left $this $right")
  }
}
case object OPlus extends BOp("+")
case object OSub extends BOp("-")
case object OMul extends BOp("*")
case object ODiv extends BOp("/")
case object OMod extends BOp("%")
case object OLt extends BOp("<")
case object OBAnd extends BOp("&")
case object OBOr extends BOp("|")
case object OBXOr extends BOp("^")
case object OLShift extends BOp("<<")
case object OSRShift extends BOp(">>")
case object OURShift extends BOp(">>>")

// Operator Parser
trait UOpParser extends RegexParsers {
  lazy val uop =
    "-" ^^^ ONeg |
      "~" ^^^ OBNot
}

// Unary Operator Parser
trait BOpParser extends RegexParsers {
  lazy val bop =
    "+" ^^^ OPlus |
      "-" ^^^ OSub |
      "*" ^^^ OMul |
      "/" ^^^ ODiv |
      "%" ^^^ OMod |
      "<" ^^^ OLt |
      "&" ^^^ OBAnd |
      "|" ^^^ OBOr |
      "^" ^^^ OBXOr |
      "<<" ^^^ OLShift |
      ">>" ^^^ OSRShift |
      ">>>" ^^^ OURShift
}

// Binary Operator Parser
trait OpParser extends UOpParser with BOpParser {
  lazy val op = uop | bop
}
