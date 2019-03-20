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

// CORE Values
trait Value

// CORE Addresses
trait Addr extends Value
case class DynAddr(long: Long) extends Addr
case class StaAddr(name: String) extends Addr

// CORE Closures
case class Clo(params: List[Id], body: Inst) extends Value

// parser for closures
trait CloParser extends InstParser {
  val clo: PackratParser[Clo] = ("(" ~> repsep(ident, ",") <~ ")" <~ "=>") ~ inst ^^ {
    case ps ~ b => Clo(ps, b)
  }
}
object Clo extends CloParser {
  def apply(str: String): Clo = parseAll(clo, str).get
}

// CORE Constants
abstract class Const(str: String) extends Value {
  override def toString: String = str
}
case class Num(n: Double) extends Const(n.toString)
case class INum(n: Long) extends Const(n.toString)
case class Str(s: String) extends Const(s.toString)
case class Bool(b: Boolean) extends Const(b.toString)
case object Undef extends Const("undefined")
case object Null extends Const("null")
