/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer.domain

import kr.ac.kaist.safe.analyzer.models.builtin.BuiltinGlobal
import kr.ac.kaist.safe.errors.error.AbsStateParseError
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.analyzer.TracePartition
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.util._

import spray.json._
import kr.ac.kaist.compabs.models.DHeap

// default state abstract domain
object DefaultState extends StateDomain {
  lazy val Bot: Elem = Elem(AbsHeap.Bot, AbsContext.Bot, DHeap.bottom)
  lazy val Top: Elem = Elem(AbsHeap.Top, AbsContext.Top, DHeap.bottom) // TODO

  def alpha(st: State): Elem = Top // TODO more precise

  def apply(heap: AbsHeap, context: AbsContext): Elem = Bot.copy(heap, context)

  def fromJson(v: JsValue): Elem = v match {
    case JsObject(m) => (
      m.get("heap").map(AbsHeap.fromJson _),
      m.get("context").map(AbsContext.fromJson _)
    ) match {
        case (Some(h), Some(c)) => Elem(h, c, DHeap.bottom) // TODO
        case _ => throw AbsStateParseError(v)
      }
    case _ => throw AbsStateParseError(v)
  }

  case class Elem(
      heap: AbsHeap,
      context: AbsContext,
      dom: DHeap.T
  ) extends ElemTrait {
    def gamma: ConSet[State] = ConInf // TODO more precise

    def getSingle: ConSingle[State] = ConMany() // TODO more precise

    def ⊑(that: Elem): Boolean =
      this.heap ⊑ that.heap && this.context ⊑ that.context && this.dom <= that.dom

    def ⊔(that: Elem): Elem =
      Elem(this.heap ⊔ that.heap, this.context ⊔ that.context, this.dom + that.dom)

    def ⊓(that: Elem): Elem = {
      throw new InternalError("TODO")
      //      Elem(this.heap ⊓ that.heap, this.context ⊓ that.context)
    }

    def raiseException(excSet: Set[Exception]): Elem = {
      if (excSet.isEmpty) Bot
      else {
        val (oldValue, _) = context.pureLocal.record.decEnvRec.GetBindingValue("@exception_all")
        val (newSt: Elem, newExcSet) = excSet.foldLeft[(Elem, AbsLoc)]((this, AbsLoc.Bot)) {
          case ((st, locSet), exc) => {
            val errModel = exc.getModel
            val errLoc = Loc(errModel.name + "<instance>")
            val newSt = st.oldify(errLoc)
            val (protoModel, _, _, _) = errModel.protoModel.get
            val newErrObj = AbsObj.newErrorObj(errModel.name, protoModel.loc)
            val retH = newSt.heap.update(errLoc, newErrObj)
            (newSt.copy(heap = retH), locSet + errLoc)
          }
        }
        val excValue = AbsValue(newExcSet)
        val localEnv = newSt.context.pureLocal
        val (envRec1, _) = localEnv.record.decEnvRec.SetMutableBinding("@exception", excValue)
        val (envRec2, _) = envRec1.SetMutableBinding("@exception_all", excValue ⊔ oldValue)
        val newCtx = newSt.context.subsPureLocal(localEnv.copy(record = envRec2))
        newSt.copy(context = newCtx)
      }
    }

    def oldify(loc: Loc): Elem = loc match {
      case Recency(_, Recent) =>
        Elem(this.heap.oldify(loc), this.context.oldify(loc), this.dom.oldify(loc))
      case _ => this
    }

    def doldify(alloc: Long): Elem = Elem(heap = heap.doldify(alloc), context.doldify(alloc), dom = this.dom.doldify(alloc))

    ////////////////////////////////////////////////////////////////
    // Lookup
    ////////////////////////////////////////////////////////////////
    def lookup_rev(id: CFGId, v: AbsPValue, absent: Boolean): Elem = {
      val x = id.text
      val localEnv = context.pureLocal
      id.kind match {
        case PureLocalVar =>
          // we can designate an exact entry by 'id'.
          val (ov, _) = localEnv.record.decEnvRec.GetBindingValue(x)
          val npv = ov.pvalue ⊓ v
          if (!npv.isBottom) {
            val nv: AbsValue = ov.copy(npv)
            val abs =
              if (absent) AbsAbsent.Top
              else AbsAbsent.Bot
            val decl = localEnv.record.decEnvRec.GetBindingValueRev(x, nv, abs)
            copy(context = context.subsPureLocal(localEnv.copy(localEnv.record.copy(decl))))
          } else {
            // Impossible case. Drop the case.
            Bot
          }
        case CapturedVar =>
          // give up.
          this
        case CapturedCatchVar =>
          // give up.
          this
        case GlobalVar =>
          val (ov, exc) = AbsGlobalEnvRec.Top.GetBindingValue(x, true)(heap)
          if (exc.isEmpty) {
            val npv = ov.pvalue ⊓ v
            if (!npv.isBottom) {
              val nv: AbsValue = ov.copy(npv)
              val abs =
                if (absent) AbsAbsent.Top
                else AbsAbsent.Bot
              val newH = AbsGlobalEnvRec.Top.GetBindingValueRev(x, nv, abs)(heap)
              this.copy(heap = newH)
            } else {
              this
            }
          } else {
            this
          }
      }
    }

    def lookup(id: CFGId): (AbsValue, Set[Exception]) = {
      val x = id.text
      val localEnv = context.pureLocal
      id.kind match {
        case PureLocalVar =>
          localEnv.record.decEnvRec.GetBindingValue(x)
        case CapturedVar =>
          AbsLexEnv.getId(localEnv.outer, x, true)(this)
        case CapturedCatchVar =>
          val collapsedEnv = context.getOrElse(PredAllocSite.COLLAPSED, AbsLexEnv.Bot)
          collapsedEnv.record.decEnvRec.GetBindingValue(x)
        case GlobalVar => AbsGlobalEnvRec.Top.GetBindingValue(x, true)(heap)
      }
    }

    def lookupBase(id: CFGId): AbsValue = {
      val x = id.text
      id.kind match {
        case PureLocalVar => AbsLoc(PredAllocSite.PURE_LOCAL)
        case CapturedVar =>
          AbsLexEnv.getIdBase(context.pureLocal.outer, x, false)(this)
        case CapturedCatchVar => AbsLoc(PredAllocSite.COLLAPSED)
        case GlobalVar => AbsLoc(BuiltinGlobal.loc)
      }
    }

    ////////////////////////////////////////////////////////////////
    // Store
    ////////////////////////////////////////////////////////////////
    def varStore(id: CFGId, value: AbsValue): Elem = {
      val x = id.text
      val localEnv = context.pureLocal
      id.kind match {
        case PureLocalVar =>
          val envRec = localEnv.record.decEnvRec
          val (newEnvRec, _) = envRec
            .CreateMutableBinding(x).fold(envRec)((e: AbsDecEnvRec) => e)
            .SetMutableBinding(x, value)
          val newEnv = localEnv.copy(record = newEnvRec)
          copy(context = context.subsPureLocal(newEnv))
        case CapturedVar =>
          val (newSt, _) = AbsLexEnv.setId(localEnv.outer, x, value, false)(this)
          newSt
        case CapturedCatchVar =>
          val env = context.getOrElse(PredAllocSite.COLLAPSED, AbsLexEnv.Bot).record.decEnvRec
          val (newEnv, _): (AbsDecEnvRec, Set[Exception]) = env
            .CreateMutableBinding(x).fold(env)((e: AbsDecEnvRec) => e)
            .SetMutableBinding(x, value)
          copy(context = context.update(PredAllocSite.COLLAPSED, AbsLexEnv(newEnv)))
        case GlobalVar =>
          val (_, newH, _) = AbsGlobalEnvRec.Top
            .SetMutableBinding(x, value, false)(heap)
          copy(heap = newH)
      }
    }

    ////////////////////////////////////////////////////////////////
    // Update location
    ////////////////////////////////////////////////////////////////
    def createMutableBinding(id: CFGId, value: AbsValue): Elem = {
      val x = id.text
      id.kind match {
        case PureLocalVar =>
          val env = context.pureLocal
          val envRec = env.record.decEnvRec
          val (newEnvRec, _) = envRec
            .CreateMutableBinding(x).fold(envRec)((e: AbsDecEnvRec) => e)
            .SetMutableBinding(x, value)
          copy(context = context.subsPureLocal(env.copy(record = newEnvRec)))
        case CapturedVar =>
          val bind = AbsBinding(value)
          val newCtx = context.pureLocal.outer.foldLeft[AbsContext](AbsContext.Bot)((tmpCtx, loc) => {
            val env = context.getOrElse(loc, AbsLexEnv.Bot)
            val envRec = env.record.decEnvRec
            val (newEnvRec, _) = envRec
              .CreateMutableBinding(x).fold(envRec)((e: AbsDecEnvRec) => e)
              .SetMutableBinding(x, value)
            tmpCtx ⊔ context.update(loc, env.copy(record = newEnvRec))
          })
          copy(context = newCtx)
        case CapturedCatchVar =>
          val collapsedLoc = PredAllocSite.COLLAPSED
          val env = context.getOrElse(collapsedLoc, AbsLexEnv.Bot)
          val envRec = env.record.decEnvRec
          val (newEnvRec, _) = envRec
            .CreateMutableBinding(x).fold(envRec)((e: AbsDecEnvRec) => e)
            .SetMutableBinding(x, value)
          copy(context = context.update(collapsedLoc, env.copy(record = newEnvRec)))
        case GlobalVar =>
          val globalLoc = BuiltinGlobal.loc
          val objV = AbsDataProp(value, AbsBool.True, AbsBool.True, AbsBool.False)
          val newHeap =
            if (AbsBool.True == heap.get(globalLoc).HasProperty(AbsStr(x), heap)) heap
            else heap.update(globalLoc, heap.get(globalLoc).update(x, objV))
          copy(heap = newHeap)
      }
    }

    ////////////////////////////////////////////////////////////////
    // delete
    ////////////////////////////////////////////////////////////////
    def delete(loc: Loc, str: String): (Elem, AbsBool) = {
      val absStr = AbsStr(str)
      val (newHeap, b1) = heap.delete(loc, absStr)
      val (newCtx, b2) = context.delete(loc, str)
      (copy(heap = newHeap, context = newCtx), b1 ⊔ b2)
    }

    def keepPartitioningIndex(tp: TracePartition): Elem = {
      val env = context.pureLocal
      val env_new = env.copy(froms = env.froms + tp)
      copy(context = context.subsPureLocal(env_new))
    }

    lazy val partitioningIndex: Set[TracePartition] = context.pureLocal.froms

    override def toString: String = toString(false)

    def toStringAll: String = toString(true)

    def toStringLoc(loc: Loc): Option[String] = heap.toStringLoc(loc) match {
      case None => context.toStringLoc(loc)
      case some => some
    }

    private def toString(all: Boolean): String = {
      "** heap **" + LINE_SEP +
        (if (all) heap.toStringAll else heap.toString) + LINE_SEP +
        LINE_SEP +
        "** context **" + LINE_SEP +
        context.toString + LINE_SEP +
        LINE_SEP +
        "** old allocation site set **" + LINE_SEP +
        context.old.toString
    }

    def toJson: JsValue = JsObject(
      ("heap", heap.toJson),
      ("context", context.toJson)
    )
  }
}
