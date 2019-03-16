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

import kr.ac.kaist.safe.LINE_SEP

// CORE Expressions
abstract class Expr {
  def appendTo(sb: StringBuilder): StringBuilder = this match {
    case ENum(n) => sb.append(n)
    case EINum(n) => sb.append(n)
    case EStr(str) => sb.append("\"").append(str).append("\"")
    case EBool(b) => sb.append(b)
    case EUndef => sb.append("undefined")
    case ENull => sb.append("null")
    case EId(id) => sb.append(id)
    case EUOp(uop, expr) =>
      sb.append("(").append(uop).append(" ")
      expr.appendTo(sb).append(")")
    case EBOp(bop, left, right) =>
      sb.append("(")
      left.appendTo(sb).append(" ").append(bop).append(" ")
      right.appendTo(sb).append(")")
    case EPropRead(obj, prop) =>
      obj.appendTo(sb).append("[")
      prop.appendTo(sb).append("]")
  }
}
case class ENum(n: Double) extends Expr
case class EINum(n: Long) extends Expr
case class EStr(str: String) extends Expr
case class EBool(b: Boolean) extends Expr
case object EUndef extends Expr
case object ENull extends Expr
case class EId(id: Id) extends Expr
case class EUOp(uop: UOp, expr: Expr) extends Expr
case class EBOp(bop: BOp, left: Expr, right: Expr) extends Expr
case class EPropRead(obj: Expr, prop: Expr) extends Expr

// parser for expressions
trait ExprParser extends IdParser with OpParser {
  lazy val num = "[0-9.]+".r ^^ { _.toDouble }
  lazy val inum = "i[0-9]+".r ^^ { _.toLong }
  lazy val str = "\"" ~> "([^\"]*)".r <~ "\""
  lazy val bool = "true" ^^^ true | "false" ^^^ false
  lazy val expr: Parser[Expr] =
    num ^^ { ENum(_) } |
      inum ^^ { EINum(_) } |
      str ^^ { EStr(_) } |
      bool ^^ { EBool(_) } |
      "undef" ^^^ EUndef |
      "null" ^^^ ENull |
      ("(" ~> uop) ~ (expr <~ ")") ^^ { case u ~ e => EUOp(u, e) } |
      ("(" ~> expr) ~ bop ~ (expr <~ ")") ^^ { case l ~ b ~ r => EBOp(b, l, r) } |
      (expr <~ "[") ~ (expr <~ "]") ^^ { case o ~ p => EPropRead(o, p) } |
      id ^^ { EId(_) }
}
