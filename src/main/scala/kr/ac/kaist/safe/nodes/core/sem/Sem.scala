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
  // interpreter for programs
  def interp(pgm: Program): Model => State = model => {
    val initialSt = State(pgm.insts, model.globals, Env(), model.heap)
    fixpoint(initialSt)
  }

  // interpreter for expressions
  def interp(expr: Expr): State => Value = st => st match {
    case State(_, _, env, heap) => expr match {
      case ENum(n) => Num(n)
      case EINum(n) => INum(n)
      case EStr(str) => Str(str)
      case EBool(b) => Bool(b)
      case EUndef => Undef
      case ENull => Null
      case EId(id) => st.getId(id)
      case EUOp(uop, expr) =>
        val v = interp(expr)(st)
        interp(uop)(v)
      case EBOp(bop, left, right) =>
        val lv = interp(left)(st)
        val rv = interp(right)(st)
        interp(bop)(lv, rv)
      case EPropRead(obj, prop) => interp(obj)(st) match {
        case (addr: Addr) => interp(prop)(st) match {
          case Str(prop) =>
            val obj = heap(addr)
            obj(prop)
          case v => error(s"not a string: $v")
        }
        case v => error(s"not an address: $v")
      }
      case EPropIn(prop, obj) => interp(prop)(st) match {
        case Str(prop) => interp(obj)(st) match {
          case (addr: Addr) =>
            val obj = heap(addr)
            obj contains prop
          case v => error(s"not an address: $v")
        }
        case v => error(s"not a string: $v")
      }
      case ETypeOf(expr) => interp(expr)(st) match {
        case (addr: Addr) =>
          val obj = heap(addr)
          obj.typeAsStr
        case Num(_) | INum(_) => Str("Number")
        case Str(_) => Str("String")
        case Bool(_) => Str("Boolean")
        case Undef => Str("Undefined")
        case Null => Str("Null")
        case Clo(_, _) => Str("Closure")
      }
    }
  }

  // interpreter for unary operators
  def interp(uop: UOp): Value => Value = (uop, _) match {
    case (ONeg, Num(n)) => Num(-n)
    case (ONeg, INum(n)) => INum(-n)
    case (ONot, Bool(b)) => Bool(!b)
    case (OBNot, INum(n)) => INum(~n)
    case (_, value) => error(s"wrong type of value for the operator $uop: $value")
  }

  // interpreter for binary operators
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
    case st @ State(insts, _, _, _) => insts match {
      case Nil => st
      case _ => fixpoint(st)
    }
  }

  // state transition
  def trans(st: State): State = st match {
    case State(insts @ (inst :: rest), globals, env, heap) => inst match {
      case IExpr(id, expr) =>
        val v = interp(expr)(st)
        val newEnv = env.setId(id, v)
        State(rest, globals, newEnv, heap)
      case IAlloc(id, ty) =>
        val (addr, newHeap) = heap.alloc(ty)
        val newEnv = env.setId(id, addr)
        State(rest, globals, newEnv, newHeap)
      case IPropWrite(obj, prop, expr) => interp(obj)(st) match {
        case (addr: Addr) => interp(prop)(st) match {
          case Str(prop) =>
            val obj = heap(addr)
            val value = interp(expr)(st)
            State(rest, globals, env, heap.update(addr, obj.update(prop, value)))
          case v => error(s"not a string: $v")
        }
        case v => error(s"not an address: $v")
      }
      case IPropDelete(obj, prop) => interp(obj)(st) match {
        case (addr: Addr) => interp(prop)(st) match {
          case Str(prop) =>
            val obj = heap(addr)
            State(rest, globals, env, heap.update(addr, obj.delete(prop)))
          case v => error(s"not a string: $v")
        }
        case v => error(s"not an address: $v")
      }
      case IFun(name, params, body) =>
        val v = Clo(params, body)
        State(rest, globals, env.setId(name, v), heap)
      case IApp(id, fun, args) => interp(fun)(st) match {
        case Clo(params, body) =>
          val vs = args.map(interp(_)(st))
          val (_, locals) = ((vs, Map[Id, Value]()) /: params) {
            case ((Nil, map), param) => (Nil, map + (param -> Undef))
            case ((value :: rest, map), param) => (rest, map + (param -> value))
          }
          val newEnv = env.copy(
            locals = locals,
            labels = Map(),
            retLabel = Some(ScopeCont(id, rest, env))
          )
          val retInst = IReturn(EUndef)
          State(List(body, retInst), globals, newEnv, heap)
        case v => error(s"not a closure: $v")
      }
      case IReturn(expr) =>
        val v = interp(expr)(st)
        env.retLabel match {
          case Some(ScopeCont(id, newInsts, newEnv)) =>
            val assEnv = newEnv.setId(id, v)
            State(newInsts, globals, assEnv, heap)
          case None => error(s"illegal return: $v")
        }
      case IIf(cond, thenInst, elseInst) => interp(cond)(st) match {
        case Bool(true) => State(thenInst :: rest, globals, env, heap)
        case Bool(false) => State(elseInst :: rest, globals, env, heap)
        case v => error(s"not a boolean: $v")
      }
      case w @ IWhile(cond, body) => interp(cond)(st) match {
        case Bool(true) => State(body :: insts, globals, env, heap)
        case Bool(false) => State(rest, globals, env, heap)
        case v => error(s"not a boolean: $v")
      }
      case ILabel(label, body) =>
        val newEnv = env.setLabel(label, LabelCont(rest, env.labels))
        val breakInst = IBreak(label)
        State(body :: breakInst :: rest, globals, newEnv, heap)
      case IBreak(label) =>
        val LabelCont(newInsts, labels) = env.getLabel(label)
        val newEnv = env.setLabelMap(labels)
        State(newInsts, globals, newEnv, heap)
      case ITry(tryInst, id) =>
        val newEnv = env.setExcLabel(ScopeCont(id, rest, env))
        State(tryInst :: rest, globals, newEnv, heap)
      case IThrow(expr) =>
        val v = interp(expr)(st)
        env.excLabel match {
          case Some(ScopeCont(id, newInsts, newEnv)) =>
            val assEnv = newEnv.setId(id, v)
            State(newInsts, globals, assEnv, heap)
          case None => error(s"uncaught exception: $v")
        }
      case ISeq(insts) => State(insts ++ rest, globals, env, heap)
      case IAssert(expr) => interp(expr)(st) match {
        case Bool(true) => State(rest, globals, env, heap)
        case Bool(false) => error(s"assertion failure: $expr")
        case v => error(s"not a boolean: $v")
      }
      case IPrint(expr) =>
        println(interp(expr)(st))
        State(rest, globals, env, heap)
      case INotYetImpl =>
        error("[NotYetImpl]")
    }
    case _ => error("no remaining instructions")
  }
}
