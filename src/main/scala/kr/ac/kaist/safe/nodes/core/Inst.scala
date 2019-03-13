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
  // Converted into string
  override def toString: String =
    stringTo(new StringBuilder).toString
  def stringTo(
    sb: StringBuilder,
    indent: String = "",
    body: Boolean = false
  ): StringBuilder = {
    if (!body) sb.append(indent)
    this match {
      case IConst(x, c) => sb.append(s"$x = $c")
      case IId(x, y) => sb.append(s"$x = $y")
      case IAlloc(x) => sb.append(s"$x = new")
      case IUOp(x, uop, y) => sb.append(s"$x = $uop $y")
      case IBOp(x, bop, y, z) => sb.append(s"$x = $y $bop $z")
      case IFun(x, ps, b) =>
        sb.append(s"$x = fun(${ps.mkString(", ")}) ")
        b.stringTo(sb, indent, true)
      case IApp(x, f, as) => sb.append(s"$x = $f(${as.mkString(", ")})")
      case ISeq(is) =>
        sb.append("{").append(LINE_SEP)
        is.foreach {
          case i =>
            i.stringTo(sb, indent + TAB)
            sb.append(LINE_SEP)
        }
        sb.append("}")
      case IPropRead(x, y, z) => sb.append(s"$x = $y[$z]")
      case IPropWrite(x, y, z) => sb.append(s"$x[$y] = $z")
      case IPropDelete(o, p) => sb.append(s"delete $o[$p]")
      case IIf(c, t, e) =>
        sb.append(s"if ($c) ")
        t.stringTo(sb, indent, true)
        sb.append(" else ")
        e.stringTo(sb, indent, true)
      case IGetProps(x, y) => sb.append(s"$x <=props= $y")
      case IWhile(c, b) =>
        sb.append(s"while ($c) ")
        b.stringTo(sb, indent, true)
      case ILabel(l) => sb.append(s"label $l")
      case IBreak(l) => sb.append(s"break $l")
      case IThrow(x) => sb.append(s"throw $x")
      case ITry(t, x, c) =>
        sb.append(s"try ")
        t.stringTo(sb, indent)
        sb.append(" catch($x) ")
        c.stringTo(sb, indent)
      case INotYetImpl => sb.append(s"???")
      case IPrint(x) => sb.append(s"print $x")
    }
  }

  // Interpretation
  def interp: State = interp(State())
  def interp(st: State): State = {
    val State(env, heap) = st
    this match {
      case IConst(x, c) =>
        val newEnv = env + (x -> c)
        State(newEnv, heap)
      case IId(x, y) =>
        val yval = env.lookup(y)
        val newEnv = env + (x -> yval)
        State(newEnv, heap)
      case IAlloc(x) =>
        val newAddr = heap.malloc
        val newHeap = heap + (newAddr -> Obj())
        State(env, newHeap)
      case IUOp(x, uop, y) =>
        val yval = env.lookup(y)
        val value = uop(yval)
        val newEnv = env + (x -> value)
        State(newEnv, heap)
      case IBOp(x, bop, y, z) =>
        val yval = env.lookup(y)
        val zval = env.lookup(z)
        val value = bop(yval, zval)
        val newEnv = env + (x -> value)
        State(newEnv, heap)
      case IFun(x, ps, b) =>
        val clo = Clo(ps, b, env)
        val newEnv = env + (x -> clo)
        State(newEnv, heap)
      case IApp(x, f, as) =>
        val fval = env.lookup(f)
        val Clo(ps, b, fenv) = cast[Clo](fval)
        if (ps.length != as.length)
          error(s"wrong number of arguments: $ps <- $as")
        val avals = as.map(env.lookup)
        val newEnv = (fenv /: (ps zip avals))(_ + _)
        b.interp(State(newEnv, heap))
      case ISeq(is) =>
        (st /: is) { case (st, i) => i.interp(st) }
      case IPropRead(x, y, z) =>
        val yval = env.lookup(y)
        val addr = cast[Addr](yval)
        val obj = heap.lookup(addr)

        val zval = env.lookup(z)
        val prop = cast[Str](zval)

        val value = obj.lookup(prop)

        val newEnv = env + (x -> value)
        State(newEnv, heap)
      case IPropWrite(x, y, z) =>
        val xval = env.lookup(x)
        val addr = cast[Addr](xval)
        val obj = heap.lookup(addr)

        val yval = env.lookup(y)
        val prop = cast[Str](yval)

        val zval = env.lookup(z)
        val newObj = obj + (prop -> zval)

        val newHeap = heap + (addr -> newObj)
        State(env, newHeap)
      case IPropDelete(x, y) =>
        val xval = env.lookup(x)
        val addr = cast[Addr](xval)
        val obj = heap.lookup(addr)

        val yval = env.lookup(y)
        val prop = cast[Str](yval)

        val newObj = obj - prop

        val newHeap = heap + (addr -> newObj)
        State(env, newHeap)
      case IIf(c, t, e) =>
        val cval = env.lookup(c)
        val Bool(b) = cast[Bool](cval)
        (if (b) t else e).interp(st)
      case IGetProps(x, y) => ???
      case w @ IWhile(c, b) =>
        val cval = env.lookup(c)
        val Bool(cond) = cast[Bool](cval)
        if (cond) w.interp(b.interp(st)) else st
      case ILabel(l) => ???
      case IBreak(l) => ???
      case IThrow(x) => ???
      case ITry(t, x, c) => ???
      case INotYetImpl => error("not yet implemented")
      case IPrint(x) =>
        println(env.lookup(x))
        st
    }
  }
}
case class IConst(lhs: Id, const: Const) extends Inst
case class IId(lhs: Id, id: Id) extends Inst
case class IAlloc(lhs: Id) extends Inst
case class IUOp(lhs: Id, uop: UOp, arg: Id) extends Inst
case class IBOp(lhs: Id, bop: BOp, left: Id, right: Id) extends Inst
case class IFun(lhs: Id, params: List[Id], body: Inst) extends Inst
case class IApp(lhs: Id, fun: Id, args: List[Id]) extends Inst
case class ISeq(insts: List[Inst]) extends Inst
case class IPropRead(lhs: Id, obj: Id, prop: Id) extends Inst
case class IPropWrite(obj: Id, prop: Id, rhs: Id) extends Inst
case class IPropDelete(obj: Id, prop: Id) extends Inst
case class IIf(cond: Id, thenI: Inst, elseI: Inst) extends Inst
case class IGetProps(lhs: Id, obj: Id) extends Inst
case class IWhile(cond: Id, body: Inst) extends Inst
case class ILabel(label: Label) extends Inst
case class IBreak(label: Label) extends Inst
case class IThrow(id: Id) extends Inst
case class ITry(tryI: Inst, id: Id, catchI: Inst) extends Inst
case object INotYetImpl extends Inst
case class IPrint(id: Id) extends Inst

// Instruction Parser
trait InstParser extends ConstParser with OpParser {
  lazy val id: Parser[String] = "([a-zA-Z_][0-9a-zA-Z_]*)".r
  lazy val label: Parser[String] = id

  lazy val inst: Parser[Inst] =
    (id <~ "=") ~ const ^^ { case x ~ c => IConst(x, c) } |
      (id <~ "=") ~ "new" ^^ { case x ~ _ => IAlloc(x) } |
      (id <~ "=") ~ uop ~ id ^^ { case x ~ u ~ y => IUOp(x, u, y) } |
      (id <~ "=") ~ id ~ bop ~ id ^^ { case x ~ y ~ b ~ z => IBOp(x, b, y, z) } |
      (id <~ "=") ~ (("fun" ~> "(" ~> repsep(id, ",")) <~ ")") ~ inst ^^ { case x ~ ps ~ b => IFun(x, ps, b) } |
      (id <~ "=") ~ id ~ ("(" ~> (repsep(id, ",") <~ ")")) ^^ { case x ~ f ~ as => IApp(x, f, as) } |
      "{" ~> (repsep(inst, ";") <~ "}") ^^ { case is => ISeq(is) } |
      (id <~ "=") ~ id ~ ("[" ~> (id <~ "]")) ^^ { case x ~ y ~ z => IPropRead(x, y, z) } |
      (id <~ "=") ~ id ^^ { case x ~ y => IId(x, y) } |
      id ~ ("[" ~> (id <~ "]")) ~ ("=" ~> id) ^^ { case x ~ y ~ z => IPropWrite(x, y, z) } |
      ("delete" ~> id) ~ ("[" ~> (id <~ "]")) ^^ { case o ~ p => IPropDelete(o, p) } |
      ("if" ~> ("(" ~> (id <~ ")"))) ~ inst ~ ("else" ~> inst) ^^ { case c ~ t ~ e => IIf(c, t, e) } |
      (id <~ "<=props=") ~ id ^^ { case x ~ y => IGetProps(x, y) } |
      ("while" ~> ("(" ~> (id <~ ")"))) ~ inst ^^ { case c ~ b => IWhile(c, b) } |
      "label" ~> label ^^ { case l => ILabel(l) } |
      "break" ~> label ^^ { case l => IBreak(l) } |
      "throw" ~> id ^^ { case x => IThrow(x) } |
      ("try" ~> inst) ~ ("catch" ~> ("(" ~> (id <~ ")"))) ~ inst ^^ { case t ~ x ~ c => ITry(t, x, c) } |
      "???" ^^^ INotYetImpl |
      "print" ~> id ^^ { case x => IPrint(x) }
}
