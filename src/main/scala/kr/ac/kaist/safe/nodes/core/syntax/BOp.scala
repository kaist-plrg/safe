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

// CORE Binary Operators
abstract class BOp(val keyword: String) extends Op
case object OPlus extends BOp("+")
case object OSub extends BOp("-")
case object OMul extends BOp("*")
case object ODiv extends BOp("/")
case object OMod extends BOp("%")
case object OLt extends BOp("<")
case object OEq extends BOp("==")
case object OBAnd extends BOp("&")
case object OBOr extends BOp("|")
case object OBXOr extends BOp("^")
case object OLShift extends BOp("<<")
case object OSRShift extends BOp(">>")
case object OURShift extends BOp(">>>")

// parser for binary operators
trait BOpParser extends CoreParser {
  val bop: PackratParser[BOp] =
    "+" ^^^ OPlus |
      "-" ^^^ OSub |
      "*" ^^^ OMul |
      "/" ^^^ ODiv |
      "%" ^^^ OMod |
      "==" ^^^ OEq |
      "&" ^^^ OBAnd |
      "|" ^^^ OBOr |
      "^" ^^^ OBXOr |
      "<<" ^^^ OLShift |
      "<" ^^^ OLt |
      ">>>" ^^^ OURShift |
      ">>" ^^^ OSRShift
}
object BOp extends BOpParser {
  def apply(str: String): BOp = parseAll(bop, str).get
}
