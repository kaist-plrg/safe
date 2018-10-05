/**
 * *****************************************************************************
 * Copyright (c) 2016-2018, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer.domain

import kr.ac.kaist.safe.analyzer.model.GLOBAL_LOC
import kr.ac.kaist.safe.util._

// symbolic value abstract domain
case class SymValue(BaseValue: ValueDomain) extends ValueDomain {
  type BaseValue = BaseValue.Elem

  // symbolic information
  sealed abstract class Info {
    override def toString: String = this match {
      case ISome(v, s) => s"(= $v || $s)"
      case INone => ""
    }
  }
  case class ISome(value: BaseValue, symset: SymSet) extends Info
  case object INone extends Info

  lazy val Bot: Elem = Elem(BaseValue.Bot, INone)
  lazy val Top: Elem = Elem(BaseValue.Top, INone)

  def alpha(value: Value): Elem = Elem(BaseValue.alpha(value))
  def apply(pvalue: AbsPValue): Elem = Elem(BaseValue(pvalue))
  def apply(locset: LocSet): Elem = Elem(BaseValue(locset))
  def apply(pvalue: AbsPValue, locset: LocSet): Elem = Elem(BaseValue(pvalue, locset))

  case class Elem(
      value: BaseValue,
      info: Info = INone
  ) extends ElemTrait {
    def gamma: ConSet[Value] = ConInf
    def getSingle: ConSingle[Value] = ConMany

    def ⊑(that: Elem): Boolean = this.value ⊑ that.value
    def ⊔(that: Elem): Elem = Elem(
      this.value ⊔ that.value,
      (this.info, that.info) match {
        case (INone, INone) => INone
        case (INone, ISome(v, s)) => ISome(this.value ⊔ v, s)
        case (ISome(v, s), INone) => ISome(v ⊔ that.value, s)
        case (ISome(lv, ls), ISome(rv, rs)) => ISome(lv ⊔ rv, ls ++ rs)
      }
    )

    val pvalue: AbsPValue = value.pvalue
    val locset: LocSet = value.locset

    def ⊓(that: Elem): Elem = Elem(this.value ⊓ that.value, INone)

    override def toString: String = s"$value$info"
    def subsLoc(from: Loc, to: Loc): Elem = Elem(value.subsLoc(from, to))
    def weakSubsLoc(from: Loc, to: Loc): Elem = Elem(value.weakSubsLoc(from, to))
    def remove(locs: Set[Loc]): Elem = Elem(value.remove(locs))
    def typeCount: Int = value.typeCount
    def getThis(h: AbsHeap): LocSet = value.getThis(h)
  }
}
