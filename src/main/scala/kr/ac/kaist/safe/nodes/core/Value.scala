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
case class Addr(addr: Long) extends Value

// CORE Closures
case class Clo(params: List[Id], body: Inst, env: Env) extends Value

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

// Constants Parser
// TODO more precise parsers
trait ConstParser extends RegexParsers {
  lazy val num: Parser[Num] = "[0-9.]+".r ^^ { case str => Num(str.toDouble) }
  lazy val inum: Parser[INum] = "i[0-9]+".r ^^ { case str => INum(str.toLong) }
  lazy val bool: Parser[Bool] = "true" ^^^ Bool(true) | "false" ^^^ Bool(false)
  lazy val str: Parser[Str] = "\"" ~> "([^\"]*)".r <~ "\"" ^^ { case str => Str(str) }
  lazy val undef: Parser[Undef.type] = "undefined" ^^^ Undef
  lazy val nul: Parser[Null.type] = "null" ^^^ Null
  lazy val const: Parser[Const] = num | bool | str | undef | nul
}
