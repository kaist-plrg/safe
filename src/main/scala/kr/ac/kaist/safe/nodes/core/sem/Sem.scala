
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

// Semantics
object Sem {
  // interpret programs
  def interp(pgm: Program): Model => State = model => {
    val Program(insts) = pgm
    val initial = State(insts, model.env, model.heap)
    fixpoint(initial)
  }

  // interpret for expressions
  def interp(expr: Expr): State => Value = st => st match {
    case State(_, env, heap) => expr match {
      case ENum(n) => Num(n)
      case EINum(n) => INum(n)
      case EStr(str) => Str(str)
      case EBool(b) => Bool(b)
      case EUndef => Undef
      case ENull => Null
      case EId(id) => env(id)
      case EUOp(uop, expr) =>
        val v = interp(expr)(st)
        interp(uop)(v)
      case EBOp(bop, left, right) =>
        val lv = interp(left)(st)
        val rv = interp(left)(st)
        interp(bop)(lv, rv)
      case EPropRead(obj, prop) => interp(obj)(st) match {
        case (addr: Addr) => interp(prop)(st) match {
          case Str(name) =>
            val obj = heap(addr)
            val prop = UserId(name)
            obj(prop)
          case v => error(s"not a string: $v")
        }
        case v => error(s"not an address: $v")
      }
    }
  }

  // interpret for unary operators
  def interp(uop: UOp): Value => Value = (uop, _) match {
    case (ONeg, Num(n)) => Num(-n)
    case (OBNot, INum(n)) => INum(~n)
    case (_, value) => error(s"wrong type of value for the operator $uop: $value")
  }

  // interpret for binary operators
  def interp(bop: BOp): (Value, Value) => Value = (bop, _, _) match {
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
    case (OPlus, INum(l), INum(r)) => INum(l + r)
    case (OSub, INum(l), INum(r)) => INum(l - r)
    case (OMul, INum(l), INum(r)) => INum(l * r)
    case (ODiv, INum(l), INum(r)) => INum(l / r)
    case (OMod, INum(l), INum(r)) => INum(l % r)
    case (OLt, INum(l), INum(r)) => Bool(l < r)
    case (OBAnd, INum(l), INum(r)) => INum(l & r)
    case (OBOr, INum(l), INum(r)) => INum(l | r)
    case (OBXOr, INum(l), INum(r)) => INum(l ^ r)
    case (OLShift, INum(l), INum(r)) => INum(l << r)
    case (OSRShift, INum(l), INum(r)) => INum(l >> r)
    case (OURShift, INum(l), INum(r)) => INum(l >>> r)

    // equality operations
    case (OEq, l, r) => Bool(l == r)
    case (_, lval, rval) => error(s"wrong type: $lval $bop $rval")
  }

  // perform transition until instructions are empty
  def fixpoint(st: State): State = trans(st) match {
    case st @ State(insts, _, _) => insts match {
      case Nil => st
      case _ => fixpoint(st)
    }
  }

  // state transition
  def trans(st: State): State = st match {
    case State(insts @ (inst :: rest), env, heap) => inst match {
      case IExpr(id, expr) =>
        val v = interp(expr)(st)
        val newEnv = env.update(id, v)
        State(rest, newEnv, heap)
      case IAlloc(id) =>
        val addr = heap.newAddr
        val newHeap = heap.update(addr, Obj())
        val newEnv = env.update(id, addr)
        State(rest, newEnv, newHeap)
      case IPropWrite(obj, prop, expr) => interp(obj)(st) match {
        case (addr: Addr) => interp(prop)(st) match {
          case Str(name) =>
            val obj = heap(addr)
            val prop = UserId(name)
            val value = interp(expr)(st)
            State(rest, env, heap.update(addr, obj.update(prop, value)))
          case v => error(s"not a string: $v")
        }
        case v => error(s"not an address: $v")
      }
      case IPropDelete(obj, prop) => interp(obj)(st) match {
        case (addr: Addr) => interp(prop)(st) match {
          case Str(name) =>
            val obj = heap(addr)
            val prop = UserId(name)
            State(rest, env, heap.update(addr, obj.delete(prop)))
          case v => error(s"not a string: $v")
        }
        case v => error(s"not an address: $v")
      }
      case IFun(name, params, body) =>
        val v = Clo(params, body)
        State(rest, env.update(name, v), heap)
      case IApp(id, fun, args) => interp(fun)(st) match {
        case Clo(params, body) =>
          val vs = args.map(interp(_)(st))
          val (_, ids) = ((vs, Map[Id, Value]()) /: params) {
            case ((Nil, map), param) => (Nil, map + (param -> Undef))
            case ((value :: rest, map), param) => (rest, map + (param -> value))
          }
          val newEnv = Env(
            ids = ids,
            retLabel = Some((id, Cont(rest, env))),
            excLabel = env.excLabel
          )
          State(body, newEnv, heap)
        case v => error(s"not a closure: $v")
      }
      case IIf(cond, thenSeq, elseSeq) => interp(cond)(st) match {
        case Bool(true) => State(thenSeq ++ rest, env, heap)
        case Bool(false) => State(elseSeq ++ rest, env, heap)
        case v => error(s"not a boolean: $v")
      }
      case w @ IWhile(cond, body) => interp(cond)(st) match {
        case Bool(true) => State(body ++ insts, env, heap)
        case Bool(false) => State(rest, env, heap)
        case v => error(s"not a boolean: $v")
      }
      case ILabel(label, body) =>
        val newEnv = env.update(label, Cont(insts, env))
        State(body, newEnv, heap)
      case IJump(label) =>
        val Cont(newInsts, newEnv) = env(label)
        State(newInsts, newEnv, heap)
      case IThrow(expr) =>
        val v = interp(expr)(st)
        env.excLabel match {
          case Some((id, Cont(newInsts, newEnv))) =>
            val assEnv = newEnv.update(id, v)
            State(newInsts, assEnv, heap)
          case None => error(s"uncaught exception: $v")
        }
      case ITry(trySeq, id, catchSeq) =>
        val newEnv = env.updateExc(id, Cont(catchSeq, env))
        State(trySeq, newEnv, heap)
      case IReturn(expr) =>
        val v = interp(expr)(st)
        env.retLabel match {
          case Some((id, Cont(newInsts, newEnv))) =>
            val assEnv = newEnv.update(id, v)
            State(newInsts, assEnv, heap)
          case None => error(s"illegal return: $v")
        }
    }
    case _ => error("no remaining instructions")
  }
}
