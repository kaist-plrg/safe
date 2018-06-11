/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.adomain

import scala.collection.immutable.HashSet

/**
 * Created by ysko on 2017. 2. 28..
 */
trait AbsDOMLocSet[V] {
  type T <: AbsDOMLocSetT[V]
  abstract class AbsDOMLocSetT[K] {
    def <=(that: T): Boolean
    def </(that: T): Boolean = !(this <= that)
    def +(that: T): T
    def +(that: K): T
    def +>(that: T): T
    def <>(that: T): T
    val isEmpty: Boolean
    val nonEmpty: Boolean = !isEmpty
    val hasNull: Boolean
    val nonNulls: T
    def contains(l: K): Boolean
    def oldify(alloc: Long): T
    def foldLeft[B](z: B)(op: (B, K) => B): B
    def /:[B](z: B)(op: (B, K) => B): B
    def filter(f: K => Boolean): T
    def partition(f: K => Boolean): (T, T)
    val isSingleton: Boolean
    val size: Int
    val head: K
  }

  val bottom: T
}

object ILocSet {
  def Make(D: AbsDOMLoc): AbsDOMLocSet[D.T] = new AbsDOMLocSet[D.T] {
    case class T(set: HashSet[D.T]) extends AbsDOMLocSetT[D.T] {
      override def <=(that: T): Boolean = {
        (this.set, that.set) match {
          case (l, r) if l eq r => true
          case (l, _) if l.isEmpty => true
          case (_, r) if r.isEmpty => false
          case (l, r) => l.subsetOf(r)
        }
      }

      override def +(that: T): T = T(this.set ++ that.set)
      override def +(that: D.T): T = T(this.set + that)
      override def +>(that: T): T = T(this.set ++ that.set)
      override def <>(that: T): T = T(this.set & that.set)
      override lazy val isEmpty: Boolean = set.isEmpty
      override lazy val hasNull: Boolean = set.contains(D.nullv)
      override lazy val nonNulls: T = T(set.filterNot(p => p == D.nullv))

      override def contains(l: D.T): Boolean = set.contains(l)

      override def oldify(alloc: Long): T = {
        if (set.contains(D.recent(alloc)))
          T(set - D.recent(alloc) + D.old(alloc))
        else this
      }

      override lazy val isSingleton: Boolean = set.size == 1
      override lazy val head: D.T = set.head
      override lazy val size: Int = set.size

      override def foldLeft[B](z: B)(op: (B, D.T) => B): B = set.foldLeft(z)(op)
      override def /:[B](z: B)(op: (B, D.T) => B): B = foldLeft[B](z)(op)

      override def filter(f: D.T => Boolean): T = {
        val nset = set.filter(f)
        if (nset.isEmpty) bottom
        else T(nset)
      }

      override def partition(f: D.T => Boolean): (T, T) = {
        val (tset, fset) = set.partition(f)
        val t1 = if (tset.isEmpty) bottom else T(tset)
        val t2 = if (fset.isEmpty) bottom else T(fset)
        (t1, t2)
      }

      override def toString: String = {
        s"{ ${set.mkString(", ")} }"
      }
    }

    val bottom: T = T(HashSet.empty[D.T])
  }
}