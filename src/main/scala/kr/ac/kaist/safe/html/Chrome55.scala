/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.safe.html

import kr.ac.kaist.compabs.models.html.IChrome55
import kr.ac.kaist.safe.BASE_DIR
import kr.ac.kaist.safe.analyzer.{ Helper, TypeConversionHelper }
import kr.ac.kaist.safe.analyzer.domain.{ AbsBool, AbsNum, AbsState, AbsStr, AbsValue, ConFin, Exception, Loc, Str, Undef }

import scala.collection.immutable.HashSet

object Chrome55 extends IChrome55 {
  override val path: String = BASE_DIR + "/src/main/resources/DOMModels/"
  override val name: String = "heap.shape"

  type AAbsState = AbsState
  type AAbsValue = AbsValue
  type AAbsBool = AbsBool
  type AAbsStr = AbsStr
  type AAbsNum = AbsNum
  type AException = Exception
  // wrapper type
  override type SemanticsFun = (AAbsValue, AAbsState) => (AAbsState, AAbsState, AAbsValue)

  case class SFInput(args: AbsValue, st: AbsState) extends SFInputT {
    def getArg(i: Int): AbsValue = Helper.propLoad(args, Set(AbsStr("0")), st.heap)
    def lenArg: AbsNum = Helper.propLoad(args, Set(AbsStr("length")), st.heap).pvalue.numval
  }

  // semantic wrapper
  override def genAPI(i: SFInput => (AAbsState, AAbsState, AAbsValue)): SemanticsFun = {
    (args: AbsValue, s: AbsState) => i(SFInput(args, s))
  }
  def nnAPI(name: String): SemanticsFun = genAPI(si => throw new InternalError(s"TODO: $name"))

  // utilities
  def returnValueA(v: AbsBool)(s: SFInput): (AbsState, AbsState, AbsValue) = returnValueC(AbsValue(v))(s)
  def returnValueB(v: AbsStr)(s: SFInput): (AbsState, AbsState, AbsValue) = returnValueC(AbsValue(v))(s)
  def returnValueC(v: AbsValue)(s: SFInput): (AbsState, AbsState, AbsValue) = returnValueD(v, AbsState.Bot)(s)
  def returnValueD(v: AbsValue, es: AbsState)(s: SFInput): (AbsState, AbsState, AbsValue) = (s.st, es, v)
  def returnValueAPI(v: AbsValue): SemanticsFun = genAPI(returnValueC(v))
  def returnValueAPI(b: Boolean): SemanticsFun = genAPI(returnValueC(AbsValue(AbsBool.alpha(b))))
  def returnValueAPI(s: String): SemanticsFun = genAPI(returnValueC(AbsValue(AbsStr.alpha(s))))
  def returnValueLocName(s: String): SemanticsFun = genAPI(returnValueC(AbsValue(Loc(s.substring(1)))))
  def returnValueIRange(l: Int, u: Int): SemanticsFun = {
    genAPI(returnValueC(AbsValue(AbsNum.alpha((l to u).map(_.toDouble).toSet))))
  }
  def returnUndef(si: SFInput): (AAbsState, AAbsState, AAbsValue) = returnValueC(AbsValue(Undef))(si)
  def toDOMString(v: AAbsValue): HashSet[String] = {
    val empty = HashSet.empty[String]
    val s_1 = if (v.pvalue.nullval.isTop) empty + null else empty
    val str = TypeConversionHelper.ToPrimitive(v).toStringSet

    val strs =
      (HashSet.empty[Str] /: str)((s_i, s) => {
        s.gamma match {
          case ConFin(ss) =>
            s_i ++ ss
          case _ => throw new InternalError("TODO")
        }
      })

    (s_1 /: strs) {
      case (s_i, Str(s)) => s_i + s
    }
  }

  override def exception(es: Set[Exception])(s: SFInput): AAbsState = {
    if (es.isEmpty) AbsState.Bot
    else s.st.raiseException(es)
  }
}
