/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.adomain

import scala.collection.immutable.HashMap
import kr.ac.kaist.compabs.models.{ DLoc, DNode }
import kr.ac.kaist.safe.analyzer.domain.Loc
import kr.ac.kaist.safe.util.{ Old, Recency, Recent }

/**
 * Created by ysko on 2017. 2. 27..
 */
trait AbsDOMHeap {
  type T <: AbsDOMT
  trait AbsDOMT {
    def <=(that: T): Boolean
    def </(that: T): Boolean = !(this <= that)
    def +>(that: T): T
    def +(that: T): T
    def domIn(i: DLoc.T): Boolean
    def lookup(i: DLoc.T): DNode.T
    def update(i: DLoc.T, o: DNode.T): T
    def oldify(l: Loc): T
    def doldify(alloc: Long): T
    def subsLoc(l_r: Recency, l_o: Recency): T
    def toString(ind: Int): String
    def diff(that: T): T
  }

  val bottom: T
}

object DOMHeap {
  def Make(): AbsDOMHeap = new AbsDOMHeap {
    case class T(map: HashMap[DLoc.T, DNode.T]) extends AbsDOMT {
      private[this] val empty = HashMap.empty[DLoc.T, DNode.T]

      override def <=(that: T): Boolean = {
        (this.map, that.map) match {
          case (l, r) if l eq r => true
          case (l, _) if l.isEmpty => true
          case (_, r) if r.isEmpty => false
          case (l, r) =>
            val set = l.keySet ++ r.keySet
            set.forall(k => this.lookup(k) <= that.lookup(k))
        }
      }

      override def </(that: T): Boolean = !(this <= that)
      override def +>(that: T): T = {
        (this.map, that.map) match {
          case (l, r) if l eq r => that
          case (l, _) if l.isEmpty => that
          case (_, r) if r.isEmpty => this
          case (l, r) =>
            val set = l.keySet ++ r.keySet
            val nmap = (empty /: set)((m_i, k) => m_i + (k -> (this.lookup(k) +> that.lookup(k))))
            T(nmap)
        }
      }
      override def +(that: T): T = {
        (this.map, that.map) match {
          case (l, r) if l eq r => that
          case (l, _) if l.isEmpty => that
          case (_, r) if r.isEmpty => this
          case (l, r) =>
            val set = l.keySet ++ r.keySet
            val nmap = (empty /: set)((m_i, k) => m_i + (k -> (this.lookup(k) + that.lookup(k))))
            T(nmap)
        }
      }
      override def domIn(i: DLoc.T): Boolean = map.contains(i)
      override def lookup(i: DLoc.T): DNode.T = map.getOrElse(i, DNode.bottom)
      override def update(i: DLoc.T, v: DNode.T): T = {
        assert(i != DLoc.nullv)
        T(map + (i -> v))
      }

      override def oldify(l: Loc): T = {
        T(map.map(o => (o._1, o._2.oldify(l))))
      }

      override def doldify(alloc: Long): T = {
        val o = DLoc.recent(alloc)
        val n = DLoc.old(alloc)
        val n_map =
          this.map.get(o) match {
            case Some(s) => this.map - o + (n -> s)
            case None => this.map
          }

        T(n_map.map(o => (o._1, o._2.doldify(alloc))))
      }

      override def subsLoc(l_r: Recency, l_o: Recency): T = {
        T(map.map(o => (o._1, o._2.subsLoc(l_r, l_o))))
      }

      override def toString(ind: Int): String = {
        val sb = new StringBuffer()
        val inds = " " * ind
        val list = map.map(kv => (s"$inds${kv._1.toString} -> ", kv._2)).toSeq.sortBy(_._1)
        list.foreach(kv => {
          val k = kv._1
          sb.append(k + "\n")
          sb.append(kv._2.toString(ind + k.length))
        })

        sb.toString
      }

      override def diff(that: T): T = {
        val locs = this.map.keySet ++ that.map.keySet
        T((HashMap.empty[DLoc.T, DNode.T] /: locs)((m_i, l) => {
          val o1 = this.lookup(l)
          val o2 = that.lookup(l)
          if (o1 </ o2 || o2 </ o1) m_i + (l -> o2)
          else m_i
        }))
      }

    }

    val bottom: T = T(HashMap.empty[DLoc.T, DNode.T])
  }
}
