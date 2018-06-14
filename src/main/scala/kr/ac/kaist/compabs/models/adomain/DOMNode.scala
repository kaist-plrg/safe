/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.adomain

import kr.ac.kaist.safe.analyzer.domain.{ AbsLoc, AbsValue, Loc, Null }
import kr.ac.kaist.compabs.models.DLoc
import kr.ac.kaist.safe.util.{ Old, Recency, Recent }
import scala.collection.immutable.HashMap

/**
 * Created by ysko on 2017. 2. 27..
 */
trait AbsDOMNode {
  type T <: AbsDOMNodeT
  trait AbsDOMNodeT {
    def <=(that: T): Boolean
    def </(that: T): Boolean = !(this <= that)
    def +>(that: T): T
    def +(that: T): T
    def lookup(p: String): AbsValue
    def lookupD(p: String, default: AbsValue): AbsValue
    def update(p: String, v: AbsValue): T
    def remove(p: String): T
    def dom(p: String): Boolean
    def oldify(a: Loc): T
    def doldify(alloc: Long): T
    def subsLoc(l_r: Recency, l_o: Recency): T
    def toString(ind: Int): String
    def addEventListener(event: String, l: Loc, bubble: Boolean): T
    def removeEventListener(event: String, l: Loc, bubble: Boolean): T
    def clearEvent(event: String): T
    def getEventHandler(event: String, bubble: Boolean): List[AbsValue]
    val eventHandlers: HashMap[(String, Boolean), List[AbsLoc]]
  }

  val bottom: T
}

object DOMNode {
  // Assume that the different type of DOM node does not have the same abstract location.
  // If they can have, we have to consider possibly absent properties.
  def Make(): AbsDOMNode = new AbsDOMNode {
    case class T(map: HashMap[String, AbsValue], handlers: HashMap[String, AbsLoc]) extends AbsDOMNodeT {
      private[this] val empty = HashMap.empty[String, AbsValue]
      private[this] val emptyHandlers: HashMap[String, AbsLoc] = HashMap.empty[String, AbsLoc]
      private[this] lazy val emptyHandler: AbsLoc = AbsLoc.Bot
      private[this] def norm(handler: HashMap[String, AbsLoc]): HashMap[String, AbsLoc] = {
        handler
      }

      private[this] def eventName(e: String, bubble: Boolean): String = {
        if (bubble) s"${e}_b"
        else s"${e}_f"
      }
      private[this] def reventName(e: String): (String, Boolean) = {
        val b =
          if (e.endsWith("_b")) true
          else if (e.endsWith("_f")) false
          else throw new InternalError("impossible case")

        (e.substring(0, e.length - 2), b)
      }

      override def addEventListener(event: String, l: Loc, bubble: Boolean): T = {
        val e = eventName(event, bubble)
        val old = handlers.getOrElse(e, AbsLoc.Bot)
        if (!old.isBottom)
          System.err.println("* Warning: Multiple event listeners.")
        //        assert(handlers.getOrElse(e, LocSet.empty).isEmpty)
        T(map, handlers + (e -> (old + l)))
      }

      override def removeEventListener(event: String, l: Loc, bubble: Boolean): T = {
        val e = eventName(event, bubble)
        val es = handlers.getOrElse(e, AbsLoc.Bot)
        if (es.isConcrete) {
          T(map, handlers + (e -> (es - l)))
        } else {
          System.err.println("* Warning: removeEventListener: cannot remove a handler.")
          this
        }
      }

      override def clearEvent(event: String): T = {
        val e = eventName(event, bubble = false)
        T(map, handlers - e)
      }

      override def getEventHandler(event: String, bubble: Boolean): List[AbsValue] = {
        // TODO need to consider the case that a handler may not exist.
        val e = eventName(event, bubble)
        val es = handlers.getOrElse(e, AbsLoc.Bot)
        if (es.isBottom) AbsValue(Null) :: Nil
        else AbsValue(es) :: Nil
      }

      lazy val eventHandlers: HashMap[(String, Boolean), List[AbsLoc]] = {
        val empty = HashMap.empty[(String, Boolean), List[AbsLoc]]
        (empty /: handlers)((m_i, kv) => {
          val es = kv._2
          if (es.isBottom) m_i
          else m_i + (reventName(kv._1) -> List[AbsLoc](es))
        })
      }

      override def <=(that: T): Boolean = {
        val b1 =
          (this.map, that.map) match {
            case (l, r) if l eq r => true
            case (l, _) if l.isEmpty => true
            case (_, r) if r.isEmpty => false
            case (l, r) =>
              val keys = l.keySet ++ r.keySet
              keys.forall(k => this.lookup(k) ⊑ that.lookup(k))
          }
        b1 && {
          (this.handlers, that.handlers) match {
            case (l, r) if l eq r => true
            case (l, _) if l.isEmpty => true
            case (_, r) if r.isEmpty => true
            case (l, r) =>
              val keys = l.keySet ++ r.keySet
              keys.forall(k => l.getOrElse(k, emptyHandler) ⊑ r.getOrElse(k, emptyHandler))
          }
        }
      }
      override def +>(that: T): T = {
        val nmap =
          (this.map, that.map) match {
            case (l, r) if l eq r => that.map
            case (l, _) if l.isEmpty => that.map
            case (_, r) if r.isEmpty => this.map
            case (l, r) =>
              val keys = l.keySet ++ r.keySet
              (empty /: keys)((m_i, k) => m_i + (k -> (this.lookup(k) ⊔ that.lookup(k))))
          }
        val nhandlers =
          (this.handlers, that.handlers) match {
            case (l, r) if l eq r => that.handlers
            case (l, _) if l.isEmpty => that.handlers
            case (_, r) if r.isEmpty => this.handlers
            case (l, r) =>
              val keys = l.keySet ++ r.keySet
              (emptyHandlers /: keys)((m_i, k) => m_i + (k -> (l.getOrElse(k, emptyHandler) ⊔ r.getOrElse(k, emptyHandler))))

          }
        T(nmap, norm(nhandlers))
      }
      override def +(that: T): T = {
        val nmap =
          (this.map, that.map) match {
            case (l, r) if l eq r => that.map
            case (l, _) if l.isEmpty => that.map
            case (_, r) if r.isEmpty => this.map
            case (l, r) =>
              val keys = l.keySet ++ r.keySet
              (empty /: keys)((m_i, k) => m_i + (k -> (this.lookup(k) ⊔ that.lookup(k))))
          }
        val nhandlers =
          (this.handlers, that.handlers) match {
            case (l, r) if l eq r => that.handlers
            case (l, _) if l.isEmpty => that.handlers
            case (_, r) if r.isEmpty => this.handlers
            case (l, r) =>
              val keys = l.keySet ++ r.keySet
              (emptyHandlers /: keys)((m_i, k) => m_i + (k -> (l.getOrElse(k, emptyHandler) ⊔ r.getOrElse(k, emptyHandler))))

          }
        T(nmap, norm(nhandlers))
      }

      override def lookup(prop: String): AbsValue = map.getOrElse(prop, AbsValue.Bot)

      override def lookupD(prop: String, default: AbsValue): AbsValue = map.getOrElse(prop, default)

      override def update(prop: String, v: AbsValue): T = T(map + (prop -> v), handlers)

      override def remove(prop: String): T = T(map - prop, handlers)

      override def dom(prop: String): Boolean = map.contains(prop)

      override def oldify(l: Loc): T =
        l match {
          case locR @ Recency(subLoc, Recent) =>
            val locO = Recency(subLoc, Old)
            T(map.map(p => (p._1, p._2.subsLoc(locR, locO))), handlers)
          case _ => this
        }

      override def doldify(alloc: Long): T = {
        copy(map.map(o => (o._1, o._2.doldify(alloc))))
      }
      override def subsLoc(l_r: Recency, l_o: Recency): T = {
        T(map.map(p => (p._1, p._2.subsLoc(l_r, l_o))), handlers.map(p => (p._1, p._2.subsLoc(l_r, l_o))))
      }

      override def toString(ind: Int): String = {
        val sb = new StringBuffer()
        val inds = " " * ind
        map.foreach(kv => {
          sb.append(s"$inds${kv._1}  -> ${kv._2}\n")
        })
        sb.append(s"$inds----------------------------------\n")
        handlers.foreach(kv => {
          sb.append(s"$inds${kv._1}  : ${kv._2}\n")
        })

        sb.toString
      }
    }

    val bottom: T = T(HashMap.empty[String, AbsValue], HashMap.empty[String, AbsLoc])
  }
}
