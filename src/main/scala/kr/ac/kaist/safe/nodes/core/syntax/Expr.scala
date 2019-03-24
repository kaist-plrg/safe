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
    case EINum(n) => sb.append("i").append(n)
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
      sb.append("(")
      obj.appendTo(sb).append("[")
      prop.appendTo(sb).append("]").append(")")
    case EPropIn(prop, obj) =>
      sb.append("(")
      prop.appendTo(sb).append(" in ")
      obj.appendTo(sb).append(")")
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
case class EPropIn(prop: Expr, obj: Expr) extends Expr

// parser for expressions
trait ExprParser extends OpParser {
  val expr: PackratParser[Expr] =
    floatingPointNumber ^^ { case s => ENum(s.toDouble) } |
      "i" ~> wholeNumber ^^ { case s => EINum(s.toLong) } |
      stringLiteral ^^ { case s => EStr(s.substring(1, s.length - 1)) } |
      "true" ^^^ EBool(true) |
      "false" ^^^ EBool(false) |
      "undefined" ^^^ EUndef |
      "null" ^^^ ENull |
      "(" ~> (uop ~ expr) <~ ")" ^^ { case u ~ e => EUOp(u, e) } |
      "(" ~> (expr ~ bop ~ expr) <~ ")" ^^ { case l ~ b ~ r => EBOp(b, l, r) } |
      "(" ~> (expr ~ ("[" ~> expr <~ "]")) <~ ")" ^^ { case o ~ p => EPropRead(o, p) } |
      "(" ~> (expr ~ ("in" ~> expr)) <~ ")" ^^ { case p ~ o => EPropIn(p, o) } |
      ident ^^ { EId(_) }
}
object Expr extends ExprParser {
  def apply(str: String): Expr = parseAll(expr, str).get
}
