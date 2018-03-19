/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.compiler

import scala.collection.immutable.HashMap
import scala.language.reflectiveCalls
import kr.ac.kaist.safe.nodes.ast._

/**
 * Created by ysko on 2016. 8. 10..
 */
object IndexCollector {
  case class Property(index: Boolean, loadIndex: Boolean, storeIndex: Boolean, asis: Boolean) {
    lazy val toScore: Int = {
      val v1 = if (index) 10 else 0
      val v2 = if (loadIndex) 1 else 0
      val v3 = if (storeIndex) 1 else 0
      val v4 = if (asis) 1 else 0
      v1 + v2 + v3 + v4
    }
  }

  val emptyProp = Property(index = false, loadIndex = false, storeIndex = false, asis = false)
  val emptyMap = HashMap.empty[Id, Property]

  type IdxMap = HashMap[Id, Property]

  def collect(pgm: ASTNode)(map: IdxMap): IdxMap = {
    walk(pgm)(map)
  }
  def collect(pgm: Option[ASTNode])(map: IdxMap): IdxMap = {
    pgm match {
      case Some(p) => walk(p)(map)
      case None => map
    }
  }

  def getVariables(map: IdxMap): IdxMap = {
    map.filter {
      case (id, Property(a, b, c, d)) =>
        //	  System.out.println(s"${id.getUniqueName} -> $a $b $c $d")
        b && (a || c || d)
      //a && b || b && c
      //        (a && b && (c || d)) || (b && c)
    }
  }

  def sorted(map: IdxMap): List[Id] = {
    map.map(p => (p._1, p._2.toScore)).toList.sortBy(p => p._1.toString(0)).sortBy(p => p._2).reverse.map(_._1)
  }

  def getID(expr: Any): Option[Id] = expr match {
    case id @ Id(i, text, Some(name), _) => Some(id)
    case VarRef(_, id) => Some(id)
    case PrefixOpApp(_, _, r) => getID(r)
    case UnaryAssignOpApp(_, lhs, _) => getID(lhs)
    case _ => None
  }

  def update(expr: Any)(f: Property => Property)(map: IdxMap): IdxMap = {
    val id = getID(expr)
    id match {
      case Some(x) => map + (x -> f(map.getOrElse(x, emptyProp)))
      case None => map
    }
  }

  def walk(nodes: List[ASTNode])(map: IdxMap): IdxMap = (map /: nodes)((m_i, s) => walk(s)(m_i))
  def walk(node: ASTNode)(map: IdxMap): IdxMap = node match {
    case NoOp(info, desc) => map
    case StmtUnit(info, stmts) => walk(stmts)(map)
    case FunDecl(info, ftn, strict) => walk(ftn)(map)
    case ABlock(info, stmts, internal) => walk(stmts)(map)
    case VarStmt(info, vds) => walk(vds)(map)
    case EmptyStmt(info) => map
    case ExprStmt(info, expr, internal) => walk(expr)(map)
    case If(info, cond, trueBranch, falseBranch) => map |> walk(cond) |> walk(trueBranch) |> whenSome(falseBranch)(walk)
    case DoWhile(info, body, cond) => map |> walk(body) |> walk(cond)
    case While(info, cond, body) => map
    case For(info, init, cond, action, body) => map
    case ForVar(info, vars, cond, action, body) => map
    case ForIn(info, lhs, expr, body) => map
    case ForVarIn(info, _var, expr, body) => map
    case Continue(info, target) => map |> whenSome(target)(walk)
    case Break(info, target) => map |> whenSome(target)(walk)
    case Return(info, expr) => map |> whenSome(expr)(walk)
    case With(info, expr, stmt) => map |> walk(expr) |> walk(stmt)
    case Switch(info, cond, frontCases, _def, backCases) => map |> walk(cond) |> walk(frontCases) |> whenSome(_def)(walk) |> walk(backCases)
    case LabelStmt(info, label, stmt) => map |> walk(label) |> walk(stmt)
    case Throw(info, expr) => map |> walk(expr)
    case Try(info, body, catchBlock, fin) => map |> walk(body) |> whenSome(catchBlock)(walk) |> whenSome(fin)(walk)
    case Debugger(info) => map
    case VarDecl(info, id, expr, strict) => map |> walk(id) |> whenSome(expr)(walk)
    case Catch(info, id, body) => map |> walk(id) |> walk(body)
    case ExprList(info, exprs) => map |> walk(exprs)
    case Cond(info, cond, trueBranch, falseBranch) => map |> walk(cond) |> walk(trueBranch) |> walk(falseBranch)
    case InfixOpApp(info, left, op, right) => map |> walk(left) |> walk(op) |> walk(right)
    case PrefixOpApp(info, op, right) => map |> walk(op) |> walk(right)
    case Program(info, body) => walk(body)(map)
    case SourceElements(_, body, _) => (map /: body)((m_i, s) => walk(s)(m_i))
    case Case(info, cond, body) => map |> walk(cond) |> walk(body)
    case UnaryAssignOpApp(info, lhs, op) =>
      map |> update(lhs)(_.copy(index = true)) |> walkLHS(lhs) |> walk(op)
    case AssignOpApp(info, lhs, Op(_, "+="), right) =>
      map |> update(lhs)(_.copy(index = true)) |> walkLHS(lhs) |> walk(right)
    case AssignOpApp(info, lhs, Op(_, "-="), right) =>
      map |> update(lhs)(_.copy(index = true)) |> walkLHS(lhs) |> walk(right)
    case AssignOpApp(info, lhs, op, right) => map |> walkLHS(lhs) |> walk(op) |> walk(right)
    case This(info) => map
    case Null(info) => map
    case Bool(info, bool) => map
    case DoubleLiteral(info, text, num) => map
    case IntLiteral(info, intVal, radix) => map
    case StringLiteral(info, quote, escaped, _) => map
    case RegularExpression(info, body, flag) => map
    case VarRef(info, id) => map |> update(id)(_.copy(asis = true)) |> walk(id)
    case ArrayExpr(info, elements) => (map /: elements)((m_i, e) => whenSome(e)(walk)(m_i))
    case ArrayNumberExpr(info, elements) => map
    case ObjectExpr(info, members) => map |> walk(members)
    case Parenthesized(info, expr) => map |> walk(expr)
    case FunExpr(info, ftn) => map |> walk(ftn)
    case Bracket(info, obj, index) =>
      (getID(index) match {
        case Some(_) =>
          map |> update(index) {
            case p @ Property(idx, true, store, asis) => p.copy(asis = true)
            case p => p.copy(loadIndex = true)
          } |> walk(obj)
        case _ => map // |> walk(obj) |> walk(index)
      }) |> walk(obj) |> walk(index)
    case Dot(info, obj, member) => map |> walk(obj) |> walk(member)
    case New(info, lhs) => map |> walk(lhs)
    case FunApp(info, fun, args) => map |> walk(fun) |> walk(args)
    case PropId(info, id) => map |> walk(id)
    case PropStr(info, str) => map
    case PropNum(info, num) => map
    case Field(info, prop, expr) => map |> walk(prop) |> walk(expr)
    case GetProp(info, prop, ftn) => map |> walk(prop) |> walk(ftn)
    case SetProp(info, prop, ftn) => map |> walk(prop) |> walk(ftn)
    case Id(info, text, uniqueName, _with) => map
    case Op(info, text) => map
    case AnonymousFnName(_, _) => map
    //    case Path(info, names) => map |> walk(names)
    case Label(info, id) => map |> walk(id)
    case Comment(info, comment) => map
    case TopLevel(_, fds, vds, stmts) => map |> walk(fds) |> walk(vds) |> walk(stmts)
    case Functional(_, fds, vds, stmts, id, params, bodyS) => map |> walk(fds) |> walk(vds) |> walk(stmts) |> walk(id) |> walk(params)
  }

  def walkLHS(node: LHS)(map: IdxMap): IdxMap = node match {
    case This(_) | Null(_) | Bool(_, _) |
      DoubleLiteral(_, _, _) | IntLiteral(_, _, _) |
      StringLiteral(_, _, _, _) | RegularExpression(_, _, _) |
      VarRef(_, _) | ArrayNumberExpr(_, _) => map
    case ArrayExpr(info, elements) => (map /: elements)((m_i, e) => whenSome(e)(walk)(m_i))
    case ObjectExpr(info, members) => map |> walk(members)
    case Parenthesized(info, expr) => map |> walk(expr)
    case FunExpr(info, ftn) => map |> walk(ftn)
    case Bracket(info, obj, index) =>
      getID(index) match {
        case Some(_) => map |> update(index)(_.copy(storeIndex = true)) |> walkLHS(obj)
        case _ => map |> walkLHS(obj) |> walk(index)
      }
    case Dot(info, obj, member) => map |> walkLHS(obj) |> walk(member)
    case New(info, lhs) => map |> walkLHS(lhs)
    case FunApp(info, fun, args) => map |> walkLHS(fun) |> walk(args)
  }
}
