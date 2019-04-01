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
    indent: String = "",
    firstIndent: Boolean = true
  ): StringBuilder = {
    if (firstIndent) sb.append(indent)
    this match {
      case IExpr(id, expr) =>
        sb.append(id).append(" = ")
        expr.appendTo(sb).append(";")
      case IAlloc(id, ty) =>
        sb.append(id).append(" = new ")
        sb.append(ty).append(";")
      case IPropWrite(obj, prop, expr) =>
        obj.appendTo(sb).append("[")
        prop.appendTo(sb).append("] = ")
        expr.appendTo(sb).append(";")
      case IPropDelete(obj, prop) =>
        sb.append("delete ")
        obj.appendTo(sb).append("[")
        prop.appendTo(sb).append("]").append(";")
      case IFun(name, params, body) =>
        sb.append("function ").append(name).append(" (")
        sb.append(params.mkString(", ")).append(") ")
        body.appendTo(sb, indent, false)
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
        sb.append(")").append(";")
      case IReturn(expr) =>
        sb.append("return ")
        expr.appendTo(sb).append(";")
      case IIf(cond, thenInst, elseInst) =>
        sb.append("if ")
        cond.appendTo(sb).append(" ")
        thenInst.appendTo(sb, indent, false)
        sb.append(" else ")
        elseInst.appendTo(sb, indent, false)
      case IWhile(cond, body) =>
        sb.append("while ")
        cond.appendTo(sb).append(" ")
        body.appendTo(sb, indent, false)
      case ILabel(label, body) =>
        sb.append("label ").append(label).append(": ")
        body.appendTo(sb, indent, false)
      case IBreak(label) =>
        sb.append("break ").append(label).append(";")
      case ITry(tryInst, id) =>
        sb.append(s"try ")
        tryInst.appendTo(sb, indent, false)
        sb.append(" catch(").append(id).append(") ")
      case IThrow(expr) =>
        sb.append("throw ")
        expr.appendTo(sb).append(";")
      case ISeq(insts) => insts match {
        case Nil => sb.append("{ }")
        case _ =>
          sb.append("{").append(LINE_SEP)
          insts.foreach(_.appendTo(sb, indent + TAB).append(LINE_SEP))
          sb.append(indent).append("}")
      }
      case IAssert(expr) =>
        sb.append("assert ")
        expr.appendTo(sb).append(";")
      case IPrint(expr) =>
        sb.append("print ")
        expr.appendTo(sb).append(";")
      case INotYetImpl => sb.append("???")
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
case class IAlloc(id: Id, ty: Ty) extends Inst
case class IPropWrite(obj: Expr, prop: Expr, expr: Expr) extends Inst
case class IPropDelete(obj: Expr, prop: Expr) extends Inst
case class IFun(name: Id, params: List[Id], body: Inst) extends Inst
case class IApp(id: Id, fun: Expr, args: List[Expr]) extends Inst
case class IReturn(expr: Expr) extends Inst
case class IIf(cond: Expr, thenInst: Inst, elseInst: Inst) extends Inst
case class IWhile(cond: Expr, body: Inst) extends Inst
case class ILabel(label: Label, body: Inst) extends Inst
case class IBreak(label: Label) extends Inst
case class ITry(tryInst: Inst, id: Id) extends Inst
case class IThrow(expr: Expr) extends Inst
case class ISeq(insts: List[Inst]) extends Inst
case class IAssert(expr: Expr) extends Inst
case class IPrint(expr: Expr) extends Inst
case object INotYetImpl extends Inst

// parser for instructions
trait InstParser extends ExprParser {
  val instSeq: PackratParser[List[Inst]] = rep(inst)
  val prop: PackratParser[String ~ Expr] = (ident <~ ":") ~ expr
  val props: PackratParser[List[String ~ Expr]] = "{" ~> repsep(prop, ",") <~ "}"
  val inst: PackratParser[Inst] =
    ("delete" ~> expr) ~ ("[" ~> (expr <~ "]")) <~ ";" ^^ { case o ~ p => IPropDelete(o, p) } |
      ("function" ~> ident) ~ (("(" ~> repsep(ident, ",")) <~ ")") ~ inst ^^ { case x ~ ps ~ b => IFun(x, ps, b) } |
      "return" ~> expr <~ ";" ^^ { case e => IReturn(e) } |
      ("if" ~> expr) ~ inst ~ ("else" ~> inst?) ^^ { case c ~ t ~ e => IIf(c, t, e.getOrElse(ISeq(Nil))) } |
      ("while" ~> expr) ~ inst ^^ { case c ~ b => IWhile(c, b) } |
      ("label" ~> ident <~ ":") ~ inst ^^ { case l ~ is => ILabel(l, is) } |
      "break" ~> ident <~ ";" ^^ { case l => IBreak(l) } |
      ("try" ~> inst) ~ ("catch" ~> ("(" ~> (ident <~ ")"))) ^^ { case t ~ x => ITry(t, x) } |
      "throw" ~> expr <~ ";" ^^ { case e => IThrow(e) } |
      "{" ~> instSeq <~ "}" ^^ { case seq => ISeq(seq) } |
      "assert" ~> expr <~ ";" ^^ { case e => IAssert(e) } |
      "print" ~> expr <~ ";" ^^ { case e => IPrint(e) } |
      "???" ^^^ { INotYetImpl } |
      expr ~ ("[" ~> expr <~ "]") ~ ("=" ~> expr) <~ ";" ^^ { case o ~ p ~ e => IPropWrite(o, p, e) } |
      (ident <~ "=" <~ "new") ~ (ident <~ ";") ^^ { case x ~ t => IAlloc(x, t) } |
      (ident <~ "=") ~ ident ~ (props <~ ";") ^^ {
        case x ~ t ~ props => ISeq(IAlloc(x, t) :: props.map {
          case p ~ e => IPropWrite(EId(x), EStr(p), e)
        })
      } |
      (ident <~ "=") ~ expr ~ ("(" ~> (repsep(expr, ",") <~ ")")) <~ ";" ^^ { case x ~ f ~ as => IApp(x, f, as) } |
      (ident <~ "=") ~ expr <~ ";" ^^ { case x ~ e => IExpr(x, e) }
}
object Inst extends InstParser {
  def apply(str: String): Inst = parseAll(inst, str).get
}
