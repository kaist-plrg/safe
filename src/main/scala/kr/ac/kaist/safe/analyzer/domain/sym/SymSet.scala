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

import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.util._
import scala.collection.immutable.HashSet
import scala.util.{ Try, Success, Failure }

// symbol set
object SymSet extends AbsDomain[Sym] {
  case object Top extends Elem
  case class SSet(set: Set[Sym]) extends Elem
  object SSet {
    def apply(seq: Sym*): SSet = SSet(seq.toSet)
  }
  lazy val Bot: Elem = SSet()

  def alpha(sym: Sym): Elem = SSet(sym)
  override def alpha(symset: Set[Sym]): Elem = SSet(symset)

  sealed abstract class Elem extends ElemTrait {
    def gamma: ConSet[Sym] = this match {
      case Top => throw SymTopGammaError // TODO ConFin(symSet)
      case SSet(set) => ConFin(set)
    }

    def getSingle: ConSingle[Sym] = this match {
      case SSet(set) if set.size == 0 => ConZero
      case SSet(set) if set.size == 1 => ConOne(set.head)
      case _ => ConMany
    }

    override def toString: String = this match {
      case Top => "Top(symbol)"
      case SSet(set) if set.size == 0 => "⊥(symbol)"
      case SSet(set) => set.mkString(", ")
    }

    def ⊑(that: Elem): Boolean = (this, that) match {
      case (_, Top) => true
      case (Top, _) => false
      case (SSet(lset), SSet(rset)) => lset subsetOf rset
    }

    def ⊔(that: Elem): Elem = (this, that) match {
      case (Top, _) | (_, Top) => Top
      case (SSet(lset), SSet(rset)) => SSet(lset ++ rset)
    }

    def ⊓(that: Elem): Elem = (this, that) match {
      case (Top, _) => that
      case (_, Top) => this
      case (SSet(lset), SSet(rset)) => SSet(lset intersect rset)
    }

    def contains(sym: Sym): Boolean = this match {
      case Top => true
      case SSet(set) => set.contains(sym)
    }

    def exists(f: Sym => Boolean): Boolean = this match {
      case Top => true
      case SSet(set) => set.exists(f)
    }

    def filter(f: Sym => Boolean): Elem = this match {
      case Top => throw SymTopGammaError // TODO SSet(symSet.filter(f))
      case SSet(set) => SSet(set.filter(f))
    }

    def foreach(f: Sym => Unit): Unit = this match {
      case Top => throw SymTopGammaError // TODO symSet.foreach(f)
      case SSet(set) => set.foreach(f)
    }

    def foldLeft[T](initial: T)(f: (T, Sym) => T): T = this match {
      case Top => throw SymTopGammaError // TODO symSet.foldLeft(initial)(f)
      case SSet(set) => set.foldLeft(initial)(f)
    }

    def map[T](f: Sym => T): Set[T] = this match {
      case Top => throw SymTopGammaError // TODO symSet.map(f)
      case SSet(set) => set.map(f)
    }

    def +(sym: Sym): Elem = this match {
      case Top => Top
      case SSet(set) => SSet(set + sym)
    }

    def -(sym: Sym): Elem = this match {
      case Top => Top // TODO SSet(symSet - sym)
      case SSet(set) => SSet(set - sym)
    }
  }
}
