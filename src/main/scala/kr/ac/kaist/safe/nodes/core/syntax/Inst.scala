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

// CORE Instructions
abstract class Inst {
  def appendTo(
    sb: StringBuilder,
    indent: String = ""
  ): StringBuilder = {
    sb.append(indent)
    this match {
      case IExpr(id, expr) =>
        sb.append(id).append(" = ")
        expr.appendTo(sb)
      case IAlloc(id) =>
        sb.append(id).append(" = new")
      case IPropWrite(obj, prop, expr) =>
        obj.appendTo(sb).append("[")
        prop.appendTo(sb).append("] = ")
        expr.appendTo(sb)
      case IPropDelete(obj, prop) =>
        sb.append("delete ")
        obj.appendTo(sb).append("[")
        prop.appendTo(sb).append("]")
      case IFun(name, params, body) =>
        sb.append("function ").append(name)
        sb.append(" (").append(params.mkString(", ")).append(") ")
        appendSeqTo(body, sb, indent)
      case IApp(id, fun, args) =>
        sb.append(id).append(" = ")
        fun.appendTo(sb).append("(")
        args match {
          case Nil =>
          case hd :: tl =>
            hd.appendTo(sb)
            tl.foreach(arg => {
              sb.append(", ")
              arg.appendTo(sb)
            })
        }
        sb.append(")")
      case IReturn(expr) =>
        sb.append("return ")
        expr.appendTo(sb)
      case IIf(cond, thenSeq, elseSeq) =>
        sb.append("if (").append(cond).append(") ")
        appendSeqTo(thenSeq, sb, indent)
        sb.append(" else ")
        appendSeqTo(elseSeq, sb, indent)
      case IWhile(cond, body) =>
        sb.append("while (").append(cond).append(") ")
        appendSeqTo(body, sb, indent)
      case ILabel(label, body) =>
        sb.append("label ")
        label.appendTo(sb).append(": ")
        appendSeqTo(body, sb, indent)
      case IJump(label) =>
        sb.append("jump ")
        label.appendTo(sb)
      case ITry(trySeq, id, catchSeq) =>
        sb.append(s"try ")
        appendSeqTo(trySeq, sb, indent)
        sb.append(" catch(").append(id).append(") ")
        appendSeqTo(catchSeq, sb, indent)
      case IThrow(expr) =>
        sb.append("throw ")
        expr.appendTo(sb)
      case IAssert(expr) =>
        sb.append("assert ")
        expr.appendTo(sb)
    }
  }

  private def appendSeqTo(
    seq: List[Inst],
    sb: StringBuilder,
    indent: String
  ): StringBuilder = seq match {
    case Nil => sb.append("{ }")
    case inst :: Nil => inst.appendTo(sb, indent)
    case _ =>
      sb.append("{").append(LINE_SEP)
      seq.foreach(_.appendTo(sb, indent + TAB).append(LINE_SEP))
      sb.append(indent).append("}")
  }
}
case class IExpr(id: Id, expr: Expr) extends Inst
case class IAlloc(id: Id) extends Inst
case class IPropWrite(obj: Expr, prop: Expr, expr: Expr) extends Inst
case class IPropDelete(obj: Expr, prop: Expr) extends Inst
case class IFun(name: Id, params: List[Id], body: List[Inst]) extends Inst
case class IApp(id: Id, fun: Expr, args: List[Expr]) extends Inst
case class IReturn(expr: Expr) extends Inst
case class IIf(cond: Expr, thenSeq: List[Inst], elseSeq: List[Inst]) extends Inst
case class IWhile(cond: Expr, body: List[Inst]) extends Inst
case class ILabel(label: Label, body: List[Inst]) extends Inst
case class IJump(label: Label) extends Inst
case class ITry(trySeq: List[Inst], id: Id, catchSeq: List[Inst]) extends Inst
case class IThrow(expr: Expr) extends Inst
case class IAssert(expr: Expr) extends Inst

// parser for instructions
trait InstParser extends ExprParser with LabelParser {
  val instSeq: PackratParser[List[Inst]] = (id <~ "=") ~ (props <~ ";") ~ instSeq ^^ {
    case x ~ props ~ seq => IAlloc(x) :: (seq /: props.reverse) {
      case (seq, p ~ e) => IPropWrite(EId(x), EStr(p), e) :: seq
    }
  } | inst ~ instSeq ^^ { case i ~ seq => i :: seq } | success(Nil)
  val block: PackratParser[List[Inst]] = inst ^^ { List(_) } | "{" ~> instSeq <~ "}"
  val prop: PackratParser[String ~ Expr] = (str <~ ":") ~ expr
  val props: PackratParser[List[String ~ Expr]] = "{" ~> repsep(prop, ",") <~ "}"
  val inst: PackratParser[Inst] =
    ("delete" ~> expr) ~ ("[" ~> (expr <~ "]")) <~ ";" ^^ { case o ~ p => IPropDelete(o, p) } |
      ("fun" ~> id) ~ (("(" ~> repsep(id, ",")) <~ ")") ~ block ^^ { case x ~ ps ~ b => IFun(x, ps, b) } |
      "return" ~> expr <~ ";" ^^ { case e => IReturn(e) } |
      ("if" ~> expr) ~ block ~ ("else" ~> block) ^^ { case c ~ t ~ e => IIf(c, t, e) } |
      ("while" ~> expr) ~ block ^^ { case c ~ b => IWhile(c, b) } |
      ("label" ~> label <~ ":") ~ block ^^ { case l ~ is => ILabel(l, is) } |
      "jump" ~> label <~ ";" ^^ { case l => IJump(l) } |
      ("try" ~> block) ~ ("catch" ~> ("(" ~> (id <~ ")"))) ~ block ^^ { case t ~ x ~ c => ITry(t, x, c) } |
      "throw" ~> expr <~ ";" ^^ { case e => IThrow(e) } |
      "assert" ~> expr <~ ";" ^^ { case e => IAssert(e) } |
      expr ~ ("[" ~> expr <~ "]") ~ ("=" ~> expr) <~ ";" ^^ { case o ~ p ~ e => IPropWrite(o, p, e) } |
      id <~ "=" <~ "new" <~ ";" ^^ { case x => IAlloc(x) } |
      (id <~ "=") ~ expr ~ ("(" ~> (repsep(expr, ",") <~ ")")) <~ ";" ^^ { case x ~ f ~ as => IApp(x, f, as) } |
      (id <~ "=") ~ expr <~ ";" ^^ { case x ~ e => IExpr(x, e) }
}
