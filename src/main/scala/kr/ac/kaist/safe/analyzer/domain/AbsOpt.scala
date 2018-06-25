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

import kr.ac.kaist.safe.analyzer.TypeConversionHelper
import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.util._
import scala.collection.immutable.{ HashMap, HashSet }

// abstract optional
case class AbsOpt[T](content: T, absent: AbsAbsent = AbsAbsent.Bot) {
  // partial order
  def ⊑(order: (T, T) => Boolean)(that: AbsOpt[T]): Boolean =
    order(this.content, that.content) && this.absent ⊑ that.absent

  // join
  def ⊔(join: (T, T) => T)(that: AbsOpt[T]): AbsOpt[T] =
    AbsOpt(join(this.content, that.content), this.absent ⊔ that.absent)

  // meet
  def ⊓(meet: (T, T) => T)(that: AbsOpt[T]): AbsOpt[T] =
    AbsOpt(meet(this.content, that.content), this.absent ⊓ that.absent)

  // existence check
  def exists(f: T => Boolean): AbsBool = {
    val trueB =
      if (!f(content)) AT
      else AbsBool.Bot
    val falseB =
      if (!absent.isBottom) AF
      else AbsBool.Bot
    trueB ⊔ falseB
  }

  override def toString: String = s"($content, $absent)"
}

object AbsOpt {
  // bottom
  def Bot[T](botV: => T): AbsOpt[T] = AbsOpt(botV, AbsAbsent.Bot)

  // top
  def Top[T](topV: => T): AbsOpt[T] = AbsOpt(topV, AbsAbsent.Top)
}
