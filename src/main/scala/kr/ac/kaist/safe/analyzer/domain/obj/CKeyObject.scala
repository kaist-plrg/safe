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

import kr.ac.kaist.safe.analyzer.models.builtin._
import kr.ac.kaist.safe.analyzer.TypeConversionHelper
import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.analyzer.domain.CKeyObject.ConsMap.FAnd
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.util._

import scala.collection.immutable.{ HashMap, HashSet }
import spray.json._

////////////////////////////////////////////////////////////////////////////////
// object abstract domain with concrete keys
////////////////////////////////////////////////////////////////////////////////
object CKeyObject extends ObjDomain {
  // Tag ::= ! | ?
  sealed abstract class Tag
  case object TBang extends Tag {
    override def toString: String = " ->"
  }
  case object TTop extends Tag {
    override def toString: String = "@->"
  }

  case class Element(s: AbsStr, tag: Tag, vm: AbsDataProp) {
    override def toString: String = s"$s $tag $vm"
  }

  abstract class Formula {
    type T <: FormulaT
    abstract class FormulaT {
      def ⊑(that: T): Boolean
      def +>(that: T): T
      def +(that: T): T
      def <>(that: T): T
      def domIn(s: AbsStr): AbsBool
      def lookup(s: AbsStr): AbsDataProp
      def lookup_kv(s: AbsStr): Set[(AbsStr, AbsDataProp)]
      def prune(s: AbsStr, propv: AbsDataProp, bPossiblyAbsent: Boolean): T
      def update(s: AbsStr, tag: Tag, vm: AbsDataProp): T
      def update_s(nk: AbsStr, nv: AbsDataProp, force: Boolean): (T, Boolean)
      def update_w(nk: AbsStr, nv: AbsDataProp, force: Boolean): T
      def delete_s(nk: AbsStr): T
      def delete_w(nk: AbsStr): T
      def subsLoc(l_r: Recency, l_o: Recency): T
      def oldify(l: Loc): T
      def map(f: (Tag, AbsDataProp) => (Tag, AbsDataProp)): T
      def toString(ind: Int): String
      val keySet: Set[AbsStr]
      val mustSet: Set[AbsStr]
      def isEmpty: Boolean
      def cons(o: T): T
    }
    val bottom: T
  }

  object ConsMap extends Formula {
    sealed trait T extends FormulaT {
      def ⊑(that: T): Boolean = {
        if (this eq that) true
        else {
          (this, that) match {
            case (FOr(list1), FOr(list2)) => list1.forall(l => list2.exists(l2 => l ⊑ l2))
            case (FOr(list1), FAnd(_, _)) => list1.forall(l => l ⊑ that)
            case (FAnd(_, _), FOr(list2)) => list2.forall(l2 => this ⊑ l2)
            case _ =>
              // may information inclusion
              this.keySet.forall(k => that.keySet.contains(k) || that.keySet.exists(k ⊑ _)) &&
                // must information inclusion
                that.mustSet.subsetOf(this.mustSet) &&
                // implication inclusion
                this.keySet.forall(k => {
                  val v1 = this.lookup(k)
                  if (v1 ⊑ AbsDataProp.Bot) true
                  else v1 ⊑ that.lookup(k)
                })
          }
        }
      }

      private def generalp(f: (T, T) => T, list: List[T]): List[T] = {
        list match {
          case h :: t if joinable(this, h) => f(this, h) :: t
          case h :: t => h :: generalp(f, t)
          case Nil => this :: Nil
        }
      }

      def +>(that: T): T = {
        if (this eq that) this
        else {
          (this, that) match {
            case (FOr(lthis), FOr(lthat)) =>
              FOr((lthat /: lthis)((lthat_i, c) => c.generalp(widen, lthat_i)))
            case (FOr(lthis), FAnd(_, _)) =>
              FOr(that.generalp(rwiden, lthis))
            case (FAnd(_, _), FOr(lthat)) =>
              FOr(this.generalp(widen, lthat))
            case (FAnd(_, _), FAnd(_, _)) =>
              widen(this, that)
          }
        }
      }

      def +(that: T): T = {
        if (this eq that) this
        else {
          cons((this, that) match {
            case (FOr(lthis), FOr(lthat)) =>
              FOr((lthat /: lthis)((lthat_i, c) => c.generalp(join, lthat_i)))
            case (FOr(lthis), FAnd(_, _)) =>
              FOr(that.generalp(join, lthis))
            case (FAnd(_, _), FOr(lthat)) =>
              FOr(this.generalp(join, lthat))
            case (FAnd(_, _), FAnd(_, _)) if joinable(this, that) =>
              join(this, that)
            case (FAnd(_, _), FAnd(_, _)) =>
              FOr(List(this, that))
          })
        }
      }

      def <>(that: T): T = throw new InternalError("Not yet implemented")
      def domIn(s: AbsStr): AbsBool = {
        val b1 =
          if (mustSet.exists(s2 => s ⊑ s2)) AbsBool.True
          else if (keySet.exists(s2 => !(s ⊓ s2).isBottom)) AbsBool.Top
          else AbsBool.Bot

        val b2 =
          if (keySet.forall(s2 => (s ⊓ s2).isBottom)) AbsBool.False
          else AbsBool.Bot

        b1 ⊔ b2
      }

      def lookupi(s: AbsStr): (Tag, AbsDataProp) = {
        this match {
          case FAnd(_, _) => throw new InternalError("impossible")
          case FOr(_) => throw new InternalError("not yet implemented")
          case _ => defBot
        }
      }
      def lookup(s: AbsStr): AbsDataProp = {
        this match {
          case FOr(list) =>
            (AbsDataProp.Bot /: list)((vm_i, l) => vm_i ⊔ l.lookup(s))
          case FAnd(map, list) if list.isEmpty =>
            s.getSingle match {
              case ConOne(_) => map.getOrElse(s, defBot)._2
              case _ =>
                var b = false
                var c2 = AbsDataProp.Bot
                val c1 =
                  (AbsDataProp.Top /: map) {
                    case (vm_i, (str, (_, vm))) if str ⊑ s =>
                      c2 = c2 ⊔ vm
                      vm_i
                    case (vm_i, (str, (_, vm))) if !(s ⊓ str).isBottom =>
                      b = true
                      vm_i ⊓ vm
                    case (vm_i, _) => vm_i
                  }
                if (b) c1
                else c2
            }

          case and @ FAnd(map, list) =>
            s.getSingle match {
              case ConOne(_) =>
                map.get(s) match {
                  case Some((_, vm)) => vm
                  case _ =>
                    val b =
                      (AbsDataProp.Top /: list) {
                        case (vm_i, (str, (_, vm))) if s ⊑ str => vm_i ⊓ vm
                        case (vm_i, _) => vm_i
                      }
                    if (AbsDataProp.Top ⊑ b) AbsDataProp.Bot
                    else b
                }
              case _ =>
                var b = false
                var c2 = AbsDataProp.Bot
                val c1 =
                  (AbsDataProp.Top /: and.toList) {
                    case (vm_i, (str, (_, vm))) if str ⊑ s =>
                      c2 = c2 ⊔ vm
                      vm_i
                    case (vm_i, (str, (_, vm))) if !(s ⊓ str).isBottom =>
                      b = true
                      vm_i ⊓ vm
                    case (vm_i, _) => vm_i
                  }
                if (b) c1 ⊔ c2
                else c2
            }
        }
      }

      def lookup_kv(s: AbsStr): Set[(AbsStr, AbsDataProp)] = {
        this match {
          case FOr(list) =>
            (Set.empty[(AbsStr, AbsDataProp)] /: list)((set_i, l) => set_i ++ l.lookup_kv(s))
          case and @ FAnd(map, list) =>
            var single: Option[(AbsStr, AbsDataProp)] = None
            s.getSingle match {
              case ConOne(_) =>
                map.get(s) match {
                  case Some((_, vm)) => Set.empty[(AbsStr, AbsDataProp)] + ((s, vm))
                  case _ =>
                    (Set.empty[(AbsStr, AbsDataProp)] /: list) {
                      case (set_i, (str, (_, vm))) if s ⊑ str => set_i + ((str, vm))
                      case (set_i, _) => set_i
                    }
                }
              case _ =>
                val nset =
                  (Set.empty[(AbsStr, AbsDataProp)] /: and.toList) {
                    case (set_i, (str, (TBang, vm))) if (str ⊑ s) && (s ⊑ str) =>
                      single = Some((str, vm))
                      set_i + ((str, vm))
                    case (set_i, (str, (TTop, vm))) if (str ⊑ s) && (s ⊑ str) =>
                      set_i + ((str, vm))
                    case (set_i, (str, (_, vm))) if !(s ⊓ str).isBottom =>
                      set_i + ((str, vm))
                    case (set_i, _) => set_i
                  }
                if (single.isDefined) single.toSet
                else nset
            }
        }
      }

      def prune(s: AbsStr, propv: AbsDataProp, bPossiblyAbsent: Boolean): T = {
        System.out.println(s"* prune: $s -> $propv")
        lazy val ntag = if (bPossiblyAbsent) TTop else TBang
        // prune(s, pv, true) =>  (s |-> ?, pv)
        // prune(s, pv, false) => (s |-> !, pv)
        // Note: compute a meet!
        this match {
          case FOr(list) =>
            val nlist = list.map(_.prune(s, propv, bPossiblyAbsent))
            val nlist_2 = nlist.filter { case and @ FAnd(_, _) if and.isEmpty => false case _ => true }
            if (nlist_2.isEmpty) throw new InternalError("impossible")
            else if (nlist_2.lengthCompare(1) == 0) nlist_2.head
            else FOr(nlist_2)
          case and @ FAnd(_, _) =>
            var bConcrete = false
            var concrete = AbsDataProp.Top
            var bot = false
            val nlist =
              (bottom /: and.toList) {
                case (list_i, (str, (tag, vm))) if str ⊑ s =>
                  val nvm = vm ⊓ propv
                  bConcrete = true
                  if (nvm ⊑ AbsDataProp.Bot) {
                    if (bPossiblyAbsent) list_i
                    else {
                      bot = true
                      list_i
                    }
                  } else {
                    list_i.update(str, tag, nvm)
                  }
                case (list_i, (str, (tag, vm))) if !(s ⊓ str).isBottom =>
                  val nvm = vm ⊓ propv
                  concrete = concrete ⊓ nvm
                  if (nvm ⊑ AbsDataProp.Bot) {
                    if (bPossiblyAbsent) list_i
                    else {
                      bot = true
                      list_i
                    }
                  } else {
                    list_i.update(str, tag, vm)
                  }
                case (list_i, (str, (tag, vm))) => list_i.update(str, tag, vm)
              }

            val nlist2 =
              if (!bConcrete && AbsDataProp.Top !⊑ concrete) nlist.update(s, ntag, concrete)
              else nlist

            if (nlist2.isEmpty) bottom
            else if (bot) bottom
            else nlist2
        }
      }

      def updatable(nk: AbsStr, nv: AbsDataProp): Boolean = {
        this match {
          case FOr(_) => throw new InternalError("not support")
          case FAnd(map, list) =>
            map.forall {
              case (str, (_, vm)) =>
                ((str ⊑ nk) && (nk ⊑ str)) ||
                  ((str ⊑ nk) && (vm ⊑ nv)) ||
                  ((nk ⊑ str) && (nv ⊑ vm)) ||
                  ((str !⊑ nk) && (nk !⊑ str))
              case _ => throw new InternalError(this.toString)
            } &&
              list.forall {
                case (str, (_, vm)) =>
                  ((str ⊑ nk) && (nk ⊑ str)) ||
                    ((str ⊑ nk) && (vm ⊑ nv)) ||
                    ((nk ⊑ str) && (nv ⊑ vm)) ||
                    ((str !⊑ nk) && (nk !⊑ str))
                case _ => throw new InternalError(this.toString)
              }
        }
      }

      def update(s: AbsStr, tag: Tag, vm: AbsDataProp): T = throw new InternalError("TODO")

      def update_s(nk: AbsStr, nv: AbsDataProp, force: Boolean): (T, Boolean) = {
        // Note: we do not consider isUpdatable because the condition is already considered.
        this match {
          case FOr(list) =>
            val (nlist, olist) = list.partition(_.updatable(nk, nv))

            if (nlist.isEmpty) {
              val nh = olist.head.update_w(nk, nv, force)
              val nlist_2 = nh match {
                case FOr(nlist_i) => nlist_i ++ olist.tail
                case _ => nh :: olist.tail
              }
              if (nlist_2.isEmpty) (bottom, true)
              else if (nlist_2.lengthCompare(1) == 0) (nlist_2.head, false)
              else (cons(FOr(nlist_2)), false)
            } else {
              val b = olist.isEmpty
              nlist.map(_.update_s(nk, nv, force)._1)
              (cons(FOr(nlist.map(_.update_s(nk, nv, force)._1) ++ olist)), b)
            }
          case FAnd(_, _) if !updatable(nk, nv) => (update_w(nk, nv, force), false)
          case FAnd(map, list) =>
            val nmap = map + (nk -> (TBang, nv))
            var strong = true
            val nlist = list.map {
              case (str, _) if str ⊑ nk =>
                (str, (TBang, nv))
              case (str, (_, vm)) if !(nk ⊓ str).isBottom =>
                if (nv ⊑ vm) {
                  strong = false
                  (str, (TTop, nv ⊔ vm))
                } else {
                  throw new InternalError("impossible)")
                }
              case p => p
            }
            (cons(FAnd(nmap, nlist)), strong)
        }
      }

      def update_w(nk: AbsStr, nv: AbsDataProp, force: Boolean): T = {
        this match {
          case FOr(list) =>
            val (nlist, olist) = list.partition(_.updatable(nk, nv))
            if (nlist.isEmpty) {
              val head = olist.head.update_w(nk, nv, force)
              head match {
                case FOr(list_i) => cons(FOr(list_i ++ olist.tail))
                case p => cons(FOr(p :: olist.tail))
              }
            } else {
              val head = nlist.head.update_w(nk, nv, force)
              head match {
                case FOr(list_i) => cons(FOr(list_i ++ nlist.tail ++ olist))
                case p => cons(FOr(p :: nlist.tail ++ olist))
              }
            }
          case FAnd(map, list) if !updatable(nk, nv) =>
            val nlist = list.filter {
              case (str, _) if !(str ⊓ nk).isBottom => false
              case _ => true
            }
            val nmap = map.filter {
              case (str, _) if str ⊑ nk => false
              case _ => true
            }
            val and = FAnd(nmap, nlist)
            val a1 = and.update(nk, TTop, nv)

            cons(FOr(a1 :: this :: Nil))

          case FAnd(map, list) =>
            var b = false
            val nlist = list.map {
              case (str, (tag, vm)) if (str ⊑ nk) && (nk ⊑ str) =>
                b = true
                val nvm = if (force || (AbsBool.True ⊑ vm.writable)) vm ⊔ nv else vm
                (str, (tag, nvm))
              case (str, (tag, vm)) if !(str ⊓ nk).isBottom =>
                val nvm = if (force || (AbsBool.True ⊑ vm.writable)) vm ⊔ nv else vm
                (str, (tag, nvm))
              case p => p
            }
            val nmap = map.map {
              case (str, (tag, vm)) if (str ⊑ nk) && (nk ⊑ str) =>
                b = true
                val nvm = if (force || (AbsBool.True ⊑ vm.writable)) vm ⊔ nv else vm
                (str, (tag, nvm))
              case (str, (tag, vm)) if !(str ⊓ nk).isBottom =>
                val nvm = if (force || (AbsBool.True ⊑ vm.writable)) vm ⊔ nv else vm
                (str, (tag, nvm))
              case p => p
            }
            val and = FAnd(nmap, nlist)
            if (b) cons(and)
            else cons(and.update(nk, TTop, nv))
          case p => throw new InternalError(p.toString)
        }
      }

      def delete_s(nk: AbsStr): T = {
        this match {
          case FOr(list) => cons(FOr(list.map(_.delete_s(nk))))
          case FAnd(map, list) =>
            val nmap = map - nk
            val nlist =
              (HashMap.empty[AbsStr, (Tag, AbsDataProp)] /: list) {
                case (map_i, (str, (_, vm))) if nk ⊑ str => map_i + (str -> (TTop, vm))
                case (_, (str, _)) if str ⊑ nk => throw new InternalError("wrong")
                case (map_i, (str, (tag, vm))) => map_i + (str -> (tag, vm))
              }
            if (nmap.isEmpty && nlist.isEmpty) bottom
            else cons(FAnd(nmap, nlist))
          case _ => this
        }
      }

      def delete_w(nk: AbsStr): T = {
        this match {
          case FOr(list) => FOr(list.map(_.delete_w(nk)))
          case FAnd(map, list) =>
            val nmap = map.map {
              case (str, (_, vm)) if str ⊑ nk =>
                (str, (TTop, vm))
              case p => p
            }
            val nlist = list.map {
              case (str, (_, vm)) if !(nk ⊓ str).isBottom =>
                (str, (TTop, vm))
              case p => p
            }
            cons(FAnd(nmap, nlist))
        }
      }

      def oldify(loc: Loc): T = loc match {
        case locR @ Recency(subLoc, Recent) => subsLoc(locR, Recency(subLoc, Old))
        case _ => this
      }

      //         weakly substitute locR by locO
      //        def weakSubsLoc(locR: Recency, locO: Recency): Elem = Elem(
      //          nmap = nmap.mapCValues { dp => dp.copy(dp.value.weakSubsLoc(locR, locO)) },
      //          imap = imap.mapCValues { iv => iv.copy(iv.value.weakSubsLoc(locR, locO)) }
      //        )

      def subsLoc(l_r: Recency, l_o: Recency): T = {
        this match {
          case and @ FAnd(_, _) => and.map((t, v) => { (t, v.copy(value = v.value.subsLoc(l_r, l_o))) })
          case FOr(list) => FOr(list.map(l => l.subsLoc(l_r, l_o)))
        }
      }

      def map(f: (Tag, AbsDataProp) => (Tag, AbsDataProp)): T = {
        this match {
          case FAnd(map, list) =>
            cons(FAnd(
              map.map {
                case (str, (tag, vm)) =>
                  val (ntag, nvm) = f(tag, vm)
                  (str, (ntag, nvm))
              },
              list.map {
                case (str, (tag, vm)) =>
                  val (ntag, nvm) = f(tag, vm)
                  (str, (ntag, nvm))
              }
            ))
          case FOr(list) => cons(FOr(list.map(_.map(f))))
        }
      }
      def isEmpty: Boolean = throw new InternalError("TODO")
      override def toString: String = toString(2)
      def toString(ind: Int): String = {
        val s = " " * ind
        this match {
          case FOr(list) =>
            list.map(_.toString(ind)).mkString(s"\n$s\\/\n$s")
          case and @ FAnd(_, _) =>
            and.toList.map(_.toString).sorted.mkString("", "\n" + " " * ind, "")
        }
      }
      lazy val keySet: Set[AbsStr] = {
        val b = this match {
          case FAnd(_, _) => throw new InternalError("impossible case")
          case FOr(list) => (Set.empty[AbsStr] /: list)((sset, l) => sset ++ l.keySet)
        }
        b
      }
      lazy val mustSet: Set[AbsStr] = {
        val b: Set[AbsStr] = this match {
          case FAnd(_, _) => throw new InternalError("impossible case")
          case FOr(h :: Nil) => h.mustSet
          case FOr(h :: t) => (h.mustSet /: t)((set_i, l) => set_i & l.mustSet)
          case _ => Set.empty
        }
        b
      }
      def cons(o: T): T = o
    }

    lazy private val defBot = (TTop, AbsDataProp.Bot)
    case class FAnd(maps: HashMap[AbsStr, (Tag, AbsDataProp)], list: HashMap[AbsStr, (Tag, AbsDataProp)]) extends T {
      override def lookupi(s: AbsStr): (Tag, AbsDataProp) = {
        s.getSingle match {
          case ConOne(_) => maps.getOrElse(s, defBot)
          case _ => list.getOrElse(s, defBot)
        }
      }
      override def update(s: AbsStr, tag: Tag, vm: AbsDataProp): T = {
        s.getSingle match {
          case ConOne(_) => FAnd(maps + (s -> (tag, vm)), list)
          case _ => FAnd(maps, list + (s -> (tag, vm)))
        }
      }
      override def isEmpty: Boolean = maps.isEmpty && list.isEmpty

      override lazy val keySet: Set[AbsStr] = maps.keySet ++ list.keySet

      override lazy val mustSet: Set[AbsStr] = {
        (Set.empty[AbsStr] /: maps) {
          case (set_i, (s, (TBang, _))) => set_i + s
          case (set_i, _) => set_i
        }
      }

      lazy val toList: List[(AbsStr, (Tag, AbsDataProp))] = {
        maps.toList ++ list.toList
      }

      lazy val values: List[(Tag, AbsDataProp)] = (list.values ++ maps.values).toList
    }

    case class FOr(list: List[T]) extends T

    protected def joinable(l1: T, l2: T): Boolean = {
      // TODO should consider "N1 = N2"
      // TODO should consider "N1 ∩ N2 ≠ ∅ ∧ V1 = V2"
      val l1abs = l1.keySet.filterNot(s => s.getSingle match { case ConOne(_) => true case _ => false })
      val l2abs = l2.keySet.filterNot(s => s.getSingle match { case ConOne(_) => true case _ => false })

      if (l1abs.isEmpty && l2abs.isEmpty) true
      else {
        l1abs == l2abs ||
          (l2abs.forall(s2 => {
            lazy val v2 = l2.lookup(s2)
            l1.updatable(s2, v2)
          }) && l1abs.forall(s1 => {
            lazy val v1 = l1.lookup(s1)
            l2.updatable(s1, v1)
          }))
      }
    }

    // Since SAFE2 does not use a widening operator, we use the join operator.
    protected def widen(l: T, r: T): T = {
      if (l eq r) l
      else {
        val keys = l.keySet ++ r.keySet
        (bottom /: keys) {
          case (map_i, s) =>
            val v = l.lookup(s) ⊔ r.lookup(s)
            val e =
              (l.lookupi(s)._1, r.lookupi(s)._1) match {
                case (TBang, TBang) => TBang
                case _ => TTop
              }
            map_i.update(s, e, v)
        }
      }
    }
    protected def rwiden(r: T, l: T): T = widen(l, r)
    protected def join(l: T, r: T): T = {
      if (l eq r) l
      else {
        val keys = l.keySet ++ r.keySet
        (bottom /: keys) {
          case (map_i, s) =>
            val v = l.lookup(s) ⊔ r.lookup(s)
            val e =
              (l.lookupi(s)._1, r.lookupi(s)._1) match {
                case (TBang, TBang) => TBang
                case _ => TTop
              }
            map_i.update(s, e, v)
        }
      }
    }
    val empty = HashMap.empty[AbsStr, (Tag, AbsDataProp)]
    val bottom: T = FAnd(empty, empty)

    def fromJson[K, V](k: JsValue => K, v: JsValue => V)(value: JsValue): ConsMap.T = throw AbsObjParseError(value)
    //      value match {
    //      case JsObject(m) => (
    //        m.get("map").map(json2map(_, k, AbsOpt.fromJson(v))),
    //        m.get("default").map(AbsOpt.fromJson(v))
    //      ) match {
    //        case (Some(m), Some(d)) => AbsMap(m, d)
    //        case _ => throw AbsMapParseError(value)
    //      }
    //      case _ => throw AbsMapParseError(value)
    //    }
  }

  lazy private[this] val ObjMapBot: ConsMap.T = ConsMap.bottom
  lazy private[this] val initIObj: AbsMap[IName, AbsIValue] = AbsMap.Bot(AbsIValue.Bot)

  // bottom / top / empty object
  lazy val Bot = Elem(ConsMap.bottom, AbsMap.Bot(AbsIValue.Bot))
  lazy val Top = Elem(ConsMap.bottom.update(AbsStr.Top, TTop, AbsDataProp.Top), AbsMap.Top(AbsIValue.Top))
  lazy val Empty = Elem(ConsMap.bottom, AbsMap.Empty(AbsIValue.Bot))

  // abstraction function
  def alpha(obj: Obj): Elem = Elem(
    map = (ConsMap.bottom /: obj.nmap) {
    case (map, (str, dp)) => map.update_s(AbsStr.alpha(str), AbsDataProp(dp), force = true)._1
  },
    imap = AbsMap(obj.imap.foldLeft[Map[IName, AbsOpt[AbsIValue]]](HashMap()) {
      case (map, (iname, iv)) => map + (iname -> AbsOpt(AbsIValue(iv)))
    }, AbsOpt(AbsIValue.Bot, AbsAbsent.Top))
  )

  // from json value to abstract object
  def fromJson(v: JsValue): Elem = throw new InternalError("TODO")
  //      v match {
  //      case JsObject(m) => (
  //        m.get("map").map(ConsMap.fromJson(json2str, AbsDataProp.fromJson)),
  //        m.get("imap").map(AbsMap.fromJson(IName.fromJson, AbsIValue.fromJson))
  //      ) match {
  //          case (Some(n), Some(i)) => Elem(n, i)
  //          case _ => throw AbsObjParseError(v)
  //        }
  //      case _ => throw AbsObjParseError(v)
  //    }

  // abstract object element
  case class Elem(
      map: ConsMap.T,
      imap: AbsMap[IName, AbsIValue]
  ) extends ElemTrait {
    // concretization function
    def gamma: ConSet[Obj] = ConInf // TODO more precise

    // get single element
    def getSingle: ConSingle[Obj] = ConMany() // TODO more precise

    // partial order
    def ⊑(that: Elem): Boolean = {
      if (this eq that) true
      else this.imap.⊑(_ ⊑ _)(that.imap) && (this.map ⊑ that.map)
    }
    // join
    def ⊔(that: Elem): Elem = {
      if (this eq that) this
      else if (this.isBottom) that
      else if (that.isBottom) this
      else Elem(this.map + that.map, this.imap.⊔(_ ⊔ _)(that.imap))
    }

    // meet
    def ⊓(that: Elem): Elem =
      Elem(this.map <> that.map, this.imap.⊓(_ ⊓ _)(that.imap))

    override def toString: String = {
      val s = new StringBuilder
      s.append("Internal Properties:").append(LINE_SEP)
        .append(imap)
        .append("Normal Properties:").append(LINE_SEP)
        .append(map)
      s.toString
    }

    ////////////////////////////////////////////////////////////////
    // Additional untility functions
    ///////////////////////////////////////////////////////////////
    def isEmpty: Boolean = this == Empty

    // oldify locations
    def oldify(loc: Loc): Elem = loc match {
      case locR @ Recency(subLoc, Recent) => subsLoc(locR, Recency(subLoc, Old))
      case _ => this
    }

    // substitute locR by locO
    def subsLoc(locR: Recency, locO: Recency): Elem = Elem(
      map = map.map { (t, v) => (t, v.copy(v.value.subsLoc(locR, locO))) },
      imap = imap.mapCValues { iv => iv.copy(iv.value.subsLoc(locR, locO)) }
    )

    // weakly substitute locR by locO
    def weakSubsLoc(locR: Recency, locO: Recency): Elem = Elem(
      map = map.map { (t, dp) => (t, dp.copy(dp.value.weakSubsLoc(locR, locO))) },
      imap = imap.mapCValues { iv => iv.copy(iv.value.weakSubsLoc(locR, locO)) }
    )

    // lookup
    private def lookup(astr: AbsStr): AbsOpt[AbsDataProp] = {
      val vm = map.lookup(astr)
      val b =
        if (AbsBool.False ⊑ map.domIn(astr)) AbsAbsent.Top
        else AbsAbsent.Bot
      AbsOpt(vm, b)
    }
    def apply(str: String): AbsDataProp = map.lookup(AbsStr(str))
    def apply(astr: AbsStr): AbsDataProp = lookup(astr).content
    def apply(iname: IName): AbsIValue = imap(iname).content

    // strong update for normal properties
    def update(str: String, dp: AbsDataProp): Elem = {
      if (dp.isBottom) Bot
      else copy(map.update_s(AbsStr.alpha(str), dp, force = false)._1)
    }

    // weak update for normal properties
    def weakUpdate(astr: AbsStr, dp: AbsDataProp): Elem = {
      if (dp.isBottom) Bot
      else copy(map.update_w(astr, dp, force = false))
    }

    // strong update for internal properties
    def update(iname: IName, iv: AbsIValue): Elem = {
      if (iv.isBottom) Bot
      else copy(imap = imap.update(iname, AbsOpt(iv, AbsAbsent.Bot)))
    }

    // strong delete
    def delete(str: String): Elem = copy(map.delete_s(AbsStr.alpha(str)))

    // weak delete
    def weakDelete(astr: AbsStr): Elem = {
      copy(map.delete_w(astr))
    }

    // contain check
    def contains(str: String): AbsBool = map.domIn(AbsStr.alpha(str))
    def contains(astr: AbsStr): AbsBool = map.domIn(astr)
    def contains(iname: IName): AbsBool = imap.contains(_.isBottom)(iname)

    // abstract key set
    def abstractKeySet: ConSet[AbsStr] = ConFin(map.keySet)

    // abstract key set with filtering
    def abstractKeySet(filter: (AbsStr, AbsDataProp) => Boolean): ConSet[AbsStr] = {
      val keys =
        (Set.empty[AbsStr] /: map.keySet) {
          case (set_i, key) =>
            if (filter(key, lookup(key).content)) set_i + key
            else set_i
        }
      ConFin(keys)
    }

    // collect key set with prefix
    def collectKeySet(prefix: String): ConSet[String] = {
      val binf = map.keySet.exists(_.gamma match { case ConInf => true case _ => false })
      if (binf) ConInf
      else {
        ConFin((Set.empty[String] /: map.keySet) {
          case (set_i, ak) => ak.gamma match {
            case ConFin(s) => set_i ++ s.map(_.str).filter(_ startsWith prefix)
            case _ => set_i
          }
        })
      }
    }

    //Section 8.12.9 [[DefineOwnProperty]](P, Desc, Throw)
    def DefineOwnProperty(P: AbsStr, Desc: AbsDesc, Throw: Boolean = true, h: AbsHeap): (Elem, AbsBool, Set[Exception]) = {
      val obj = this
      val Reject =
        if (Throw) (obj, AbsBool.Bot, HashSet(TypeError))
        else (obj, AbsBool.False, ExcSetEmpty)
      val BotTriple = (Bot, AbsBool.Bot, ExcSetEmpty)
      // 1. Let current be the result of calling the [[GetOwnProperty]] internal method of O with property name P.
      val (current, curUndef) = GetOwnProperty(P)
      // 2. Let extensible be the value of the [[Extensible]] internal property of O.
      val extensible = obj(IExtensible).value.pvalue.boolval

      val (obj1, b1, excSet1) = if (curUndef.isTop) {
        val (obj1, b1, excSet1) =
          // 4. If current is undefined and extensible is true, then
          if (AbsBool.True ⊑ extensible) {
            // i. Create an own data property named P of object O whose [[Value]], [[Writable]],
            // [[Enumerable]] and [[Configurable]] attribute values are described by Desc.
            val changedObj = weakUpdate(P, AbsDataProp(Desc))
            (changedObj, AbsBool.True, ExcSetEmpty)
          } else BotTriple
        val (obj2: Elem, b2, excSet2: Set[Exception]) =
          // 3. If current is undefined and extensible is false, then Reject.
          if (AbsBool.False ⊑ extensible) Reject
          else BotTriple
        (obj1 ⊔ obj2, b1 ⊔ b2, excSet1 ++ excSet2)
      } else BotTriple

      val (cv, cva) = current.value
      val (cw, cwa) = current.writable
      val (ce, cea) = current.enumerable
      val (cc, cca) = current.configurable
      val (dv, dva) = Desc.value
      val (dw, dwa) = Desc.writable
      val (de, dea) = Desc.enumerable
      val (dc, dca) = Desc.configurable

      // 5. Return true, if every field in Desc is absent.
      val (obj5, b5) =
        if (dva.isTop && dwa.isTop && dea.isTop && dca.isTop) (obj, AbsBool.True)
        else (Bot, AbsBool.Bot)
      // 6. Return true, if every field in Desc also occurs in current and the value of every field in Desc is the
      // same value as the corresponding field in current when compared using the SameValue algorithm (9.12).
      val (obj6, b6) =
        if ((dva.isTop || (!dv.isBottom && (AbsBool.True ⊑ TypeConversionHelper.SameValue(h, dv, cv)))) &&
          (dwa.isTop || (!dw.isBottom && (AbsBool.True ⊑ (dw StrictEquals cw)))) &&
          (dea.isTop || (!de.isBottom && (AbsBool.True ⊑ (de StrictEquals ce)))) &&
          (dca.isTop || (!dc.isBottom && (AbsBool.True ⊑ (dc StrictEquals cc))))) (obj, AbsBool.True)
        else (Bot, AbsBool.Bot)

      // 7. If the [[Configurable]] field of current is false then
      val (obj2: Elem, b2, excSet2: Set[Exception]) =
        if (AbsBool.False ⊑ cc) {
          // a. Reject, if the [[Configurable]] field of Desc is true.
          if (AbsBool.True ⊑ dc) Reject
          // b. Reject, if the [[Enumerable]] field of Desc is present and the [[Enumerable]] fields of current and
          // Desc are the Boolean negation of each other.
          else if (!de.isBottom &&
            ((AbsBool.True ⊑ de) && (AbsBool.False ⊑ ce) || (AbsBool.True ⊑ ce) && (AbsBool.False ⊑ de))) Reject
          else BotTriple
        } else BotTriple

      // 10. Else, if IsDataDescriptor(current) and IsDataDescriptor(Desc) are both true, then
      val (obj7: AbsObj, b7, excSet7: Set[Exception]) =
        // a. If the [[Configurable]] field of current is false, then
        if (AbsBool.False ⊑ cc) {
          // i. Reject, if the [[Writable]] field of current is false and the [[Writable]] field of Desc is true.
          if ((AbsBool.False ⊑ cw) && (AbsBool.True ⊑ dw)) Reject
          // ii. If the [[Writable]] field of current is false, then
          else if ((AbsBool.False ⊑ cw) &&
            // 1. Reject, if the [[Value]] field of Desc is present and SameValue(Desc.[[Value]],
            // current.[[Value]]) is false.
            !dv.isBottom && (AbsBool.False ⊑ TypeConversionHelper.SameValue(h, dv, cv))) Reject
          else BotTriple
        } else BotTriple

      // 12. For each attribute field of Desc that is present, set the correspondingly named attribute of the
      // property named P of object O to the value of the field.
      val (obj3, b3, excSet3) =
        if ((AbsBool.True ⊑ cc) || (AbsBool.True ⊑ cw)) {
          var newDP = obj(P)
          if (!dv.isBottom) newDP = newDP.copy(value = dv)
          if (!dw.isBottom) newDP = newDP.copy(writable = dw)
          if (!de.isBottom) newDP = newDP.copy(enumerable = de)
          if (!dc.isBottom) newDP = newDP.copy(configurable = dc)
          val changedObj = copy(map.update_w(P, newDP, force = true))
          (changedObj, AbsBool.True, ExcSetEmpty)
        } else BotTriple

      val excSet4 =
        if ((AbsStr("Array") ⊑ obj(IClass).value.pvalue.strval) &&
          (AbsStr("length") ⊑ P) &&
          (AbsBool.False ⊑ (TypeConversionHelper.ToNumber(dv) StrictEquals TypeConversionHelper.ToUint32(dv)))) HashSet(RangeError)
        else ExcSetEmpty

      val excSet5 =
        if ((AbsStr("Array") ⊑ obj(IClass).value.pvalue.strval) &&
          (AbsStr("length") ⊑ P) &&
          (AbsBool.False ⊑ obj(AbsStr.Number).configurable)) HashSet(TypeError)
        else ExcSetEmpty

      val excSet6 =
        if ((AbsStr("Array") ⊑ obj(IClass).value.pvalue.strval) &&
          (AT ⊑ (P StrictEquals AbsStr.Number)) &&
          (AbsBool.False ⊑ obj("length").writable)) HashSet(TypeError)
        else ExcSetEmpty

      // TODO: unsound. Should Reject if an array element could not be deleted, i.e., it's not configurable.
      // TODO: Implement DefineOwnProperty for Array objects (15.4.5.1).

      (
        obj1 ⊔ obj2 ⊔ obj3 ⊔ obj5 ⊔ obj6 ⊔ obj7, b1 ⊔ b2 ⊔ b3 ⊔ b5 ⊔ b6 ⊔ b7,
        excSet1 ++ excSet2 ++ excSet3 ++ excSet4 ++ excSet5 ++ excSet6 ++ excSet7
      )
    }

    // Section 8.12.5 [[Put]](P, V, Throw)
    def Put(P: AbsStr, V: AbsValue, Throw: Boolean = true, h: AbsHeap): (Elem, Set[Exception]) = {
      val canPut = CanPut(P, h)
      val (falseObj: Elem, falseExcSet: Set[Exception]) =
        if (AbsBool.False ⊑ canPut) {
          if (Throw) (Bot, HashSet(TypeError))
          else (this, ExcSetEmpty)
        } else (Bot, ExcSetEmpty)
      val (trueObj, trueExcSet) =
        if (AbsBool.True ⊑ canPut) {
          val (ownDesc, ownUndef) = GetOwnProperty(P)
          val (ownObj, ownExcSet) =
            if (ownDesc.isBottom) (Bot, ExcSetEmpty)
            else {
              val valueDesc = AbsDesc((V, AbsAbsent.Bot))
              val (o2, _, e) = DefineOwnProperty(P, valueDesc, Throw, h)
              (o2, e)
            }
          val (undefObj, undefExcSet) =
            if (ownUndef.isBottom) (Bot, ExcSetEmpty)
            else {
              val desc = GetProperty(P, h)
              val newDesc = AbsDesc(
                (V, AbsAbsent.Bot),
                (AbsBool.True, AbsAbsent.Bot),
                (AbsBool.True, AbsAbsent.Bot),
                (AbsBool.True, AbsAbsent.Bot)
              )
              val (o2, _, e) = DefineOwnProperty(P, newDesc, Throw, h)
              (o2, e)
            }
          (ownObj ⊔ undefObj, ownExcSet ++ undefExcSet)
        } else (Bot, ExcSetEmpty)
      (falseObj ⊔ trueObj, falseExcSet ++ trueExcSet)
    }

    // Section 8.12.4 [[CanPut]](P)
    def CanPut(P: AbsStr, h: AbsHeap): AbsBool = {
      val (desc, undef) = GetOwnProperty(P)
      val (b, _) = desc.writable
      val newB = {
        if (undef.isBottom) AbsBool.Bot
        else {
          val proto = this(IPrototype).value
          val extensible = this(IExtensible).value.pvalue.boolval
          val nullProtoCase =
            if (proto.pvalue.nullval.isBottom) AbsBool.Bot
            else extensible
          val (inheritDesc, inheritUndef) = proto.locset.foldLeft((AbsDesc.Bot, AbsUndef.Bot)) {
            case ((desc, undef), loc) => {
              val (newDesc, newUndef) = GetProperty(P, h)
              (desc ⊔ newDesc, undef ⊔ newUndef)
            }
          }
          val undefInheritCase =
            if (inheritUndef.isBottom) AbsBool.Bot
            else extensible
          val b = extensible
          val f =
            if (AF ⊑ b) {
              AbsBool.False
            } else AbsBool.Bot
          val t =
            if (AT ⊑ b) {
              val (b, _) = inheritDesc.writable
              b
            } else AbsBool.Bot
          val inheritCase = t ⊔ f
          nullProtoCase ⊔ undefInheritCase ⊔ inheritCase
        }
      }
      b ⊔ newB
    }

    // Section 8.12.3 [[Get]](P)
    def Get(astr: AbsStr, h: AbsHeap): AbsValue = {
      var visited = HashSet[Loc]()
      val valueBot = AbsValue.Bot
      def visit(currentObj: Elem): AbsValue = {
        val test = currentObj contains astr
        val v1 =
          if (AbsBool.True ⊑ test) currentObj(astr).value
          else valueBot
        val v2 =
          if (AbsBool.False ⊑ test) {
            val protoV = currentObj(IPrototype).value
            val v3 = protoV.pvalue.nullval.fold(valueBot)({ _ =>
              AbsUndef.Top
            })
            v3 ⊔ protoV.locset.foldLeft(valueBot)((v, protoLoc) => {
              if (visited contains protoLoc) v
              else {
                visited += protoLoc
                v ⊔ visit(h.get(protoLoc))
              }
            })
          } else valueBot
        v1 ⊔ v2
      }
      visit(this)
    }

    val emptyMap = HashMap.empty[AbsStr, AbsValue]
    def Get_case(astr: AbsStr, h: AbsHeap): (HashMap[AbsStr, AbsValue], HashMap[AbsStr, AbsValue]) = {
      var set_n = emptyMap
      var set_a = emptyMap

      var visited = HashSet[Loc]()
      def visit(currentObj: Elem): Unit = {
        val test = currentObj contains astr
        if (AbsBool.True ⊑ test) {
          val set = currentObj.map.lookup_kv(astr)
          set.foreach {
            case (k, v) => set_n += k -> v.value
          }
        }

        if (AbsBool.False ⊑ test) {
          val protoV = currentObj(IPrototype).value
          protoV.pvalue.nullval.foldUnit(_ => set_a += astr -> AbsUndef.Top)

          protoV.locset.foreach(protoLoc => {
            if (!(visited contains protoLoc)) {
              visited += protoLoc
              visit(h.get(protoLoc))
            }
          })
        }
      }
      visit(this)

      (set_n, set_a)
    }

    // Section 8.12.2 [[GetProperty]](P)
    def GetProperty(P: AbsStr, h: AbsHeap): (AbsDesc, AbsUndef) = {
      var visited = HashSet[Loc]()
      def visit(currObj: Elem): (AbsDesc, AbsUndef) = {
        val (desc, undef) = currObj.GetOwnProperty(P)
        val (parentDesc, parentUndef) =
          if (undef.isBottom) (AbsDesc.Bot, AbsUndef.Bot)
          else {
            val proto = currObj(IPrototype)
            val undef =
              if (proto.value.pvalue.nullval.isBottom) AbsUndef.Top
              else AbsUndef.Bot
            proto.value.locset.foldLeft((AbsDesc.Bot, AbsUndef.Bot)) {
              case ((desc, undef), loc) => {
                if (visited contains loc) (desc, undef)
                else {
                  visited += loc
                  val (newDesc, newUndef) = visit(h.get(loc))
                  (desc ⊔ newDesc, undef ⊔ newUndef)
                }
              }
            }
          }
        (desc ⊔ parentDesc, undef ⊔ parentUndef)
      }
      visit(this)
    }

    // Section 8.12.6 [[HasProperty]](P)
    def HasProperty(P: AbsStr, h: AbsHeap): AbsBool = {
      var visited = AbsLoc.Bot
      def visit(currObj: Elem): AbsBool = {
        val test = currObj contains P
        val b1 =
          if (AbsBool.True ⊑ test) AbsBool.True
          else AbsBool.Bot
        val b2 =
          if (AbsBool.False ⊑ test) {
            val protoV = currObj(IPrototype).value
            val b3 = protoV.pvalue.nullval.fold(AbsBool.Bot) { _ => AbsBool.False }
            b3 ⊔ protoV.locset.foldLeft[AbsBool](AbsBool.Bot)((b, protoLoc) =>
              if (visited contains protoLoc) b
              else {
                visited += protoLoc
                b ⊔ visit(h.get(protoLoc))
              })
          } else AbsBool.Bot
        b1 ⊔ b2
      }
      visit(this)
    }

    // key set pair
    def keySetPair(h: AbsHeap): (List[String], AbsStr) = {
      var visited = HashSet[Loc]()
      def visit(currObj: Elem): (Set[String], AbsStr) = {
        val pair = currObj.ownKeySetPair
        val proto = currObj(IPrototype)
        proto.value.locset.foldLeft(pair) {
          case ((strSet, astr), loc) => {
            if (visited contains loc) (strSet, astr)
            else {
              visited += loc
              val (newStrSet, newAStr) = visit(h.get(loc))
              (strSet ++ newStrSet, astr ⊔ newAStr)
            }
          }
        }
      }
      val (strSet, astr) = visit(this)
      (strSet.toList.sortBy { _.toString }, astr) // TODO for-in order
    }

    private def ownKeySetPair: (Set[String], AbsStr) = {
      ((HashSet.empty[String], AbsStr.Bot) /: map.keySet) {
        case ((strSet, astr), key) =>
          val av = lookup(key)
          val isEnum = av.content.enumerable
          if (AbsBool.True ⊑ isEnum) {
            val isDef = map.mustSet.contains(key)
            if (isDef && (AbsBool.Top != isEnum)) {
              val s = key.getSingle match {
                case ConOne(v) => v
                case _ => throw new InternalError("impossible")
              }
              (strSet + s, astr)
            } else (strSet, astr ⊔ key)
          } else (strSet, astr)
      }
    }

    def isDefinite(astr: AbsStr): Boolean = map.mustSet.contains(astr)

    ////////////////////////////////////////////////////////////////
    // internal methods of ECMAScript Object
    ///////////////////////////////////////////////////////////////
    // Section 8.12.1 [[GetOwnProperty]](P)
    def GetOwnProperty(P: AbsStr): (AbsDesc, AbsUndef) = {
      val AbsOpt(dp, absent) = lookup(P)
      val undef =
        if (absent.isBottom) AbsUndef.Bot else AbsUndef.Top
      val desc = AbsDesc(
        (dp.value, AbsAbsent.Bot),
        (dp.writable, AbsAbsent.Bot),
        (dp.enumerable, AbsAbsent.Bot),
        (dp.configurable, AbsAbsent.Bot)
      )
      (desc, undef)
    }

    // Section 8.12.7 [[Delete]](P, Throw)
    def Delete(astr: AbsStr, Throw: Boolean): (Elem, AbsBool, Set[Exception]) = {
      val BOT = (Bot, AbsBool.Bot, ExcSetEmpty)
      val (desc, undef) = GetOwnProperty(astr)
      val (undefO, undefB, undefE) =
        if (undef.isBottom) BOT
        else (this, AT, ExcSetEmpty)
      val (descO, descB, descE) =
        if (desc.isBottom) BOT
        else {
          val (configurable, _) = desc.configurable
          val (confO, conB, confE) =
            if (AT ⊑ configurable) (weakDelete(astr), AT, ExcSetEmpty)
            else BOT
          val (otherO, otherB, otherE: Set[Exception]) =
            if (AF ⊑ configurable) if (Throw) (Bot, AbsBool.Bot, HashSet(TypeError)) else (this, AF, ExcSetEmpty)
            else BOT
          (confO ⊔ otherO, conB ⊔ otherB, confE ++ otherE)
        }
      (undefO ⊔ descO, undefB ⊔ descB, undefE ++ descE)
    }

    // Section 8.12.8 [[DefaultValue]](hint)
    def DefaultValue(hint: String, h: AbsHeap): AbsPValue = {
      hint match {
        case "String" => DefaultValueAsString(h)
        case "Number" => DefaultValueAsNumber(h)
        case _ => DefaultValue(h)
      }
    }

    def DefaultValue(h: AbsHeap): AbsPValue = {
      val className = this(IClass)
      val isDateClass = className.value.pvalue.strval StrictEquals AbsStr("Date")
      val b = isDateClass
      val t =
        if (AT ⊑ b) {
          DefaultValueAsString(h)
        } else AbsPValue.Bot
      val f =
        if (AF ⊑ b) {
          DefaultValueAsNumber(h)
        } else AbsPValue.Bot
      t ⊔ f
    }

    private def DefaultValueAsString(h: AbsHeap): AbsPValue = {
      val toString = Get("toString", h)
      val isCallable = TypeConversionHelper.IsCallable(toString, h)
      val str =
        if (AbsBool.True ⊑ isCallable) AbsPValue(strval = AbsStr.Top)
        else AbsPValue.Bot
      if (AbsBool.False ⊑ isCallable) {
        val valueOf = Get("valueOf", h)
        val value =
          if (AbsBool.True ⊑ TypeConversionHelper.IsCallable(valueOf, h)) AbsPValue.Top
          else AbsPValue.Bot
        str ⊔ value
      } else str
    }

    private def DefaultValueAsNumber(h: AbsHeap): AbsPValue = {
      val valueOf = Get("valueOf", h)
      val isCallable = TypeConversionHelper.IsCallable(valueOf, h)
      val value =
        if (AbsBool.True ⊑ isCallable) AbsPValue.Top
        else AbsPValue.Bot
      if (AbsBool.False ⊑ isCallable) {
        val toString = Get("toString", h)
        val str =
          if (AbsBool.True ⊑ TypeConversionHelper.IsCallable(toString, h)) AbsPValue(strval = AbsStr.Top)
          else AbsPValue.Bot
        value ⊔ str
      } else value
    }
    def toJson: JsValue = throw new InternalError("TODO")
    //    JsObject(
    //      ("nmap", nmap.toJson(JsString(_), _.toJson)),
    //      ("imap", imap.toJson(_.toJson, _.toJson))
    //    )
  }

  ////////////////////////////////////////////////////////////////
  // new Object constructors
  ////////////////////////////////////////////////////////////////
  def newObject: Elem = newObject(BuiltinObjectProto.loc)

  def newObject(loc: Loc): Elem = newObject(AbsLoc(loc))

  def newObject(locSet: AbsLoc): Elem = {
    Empty
      .update(IClass, AbsIValue(AbsStr("Object")))
      .update(IPrototype, AbsIValue(locSet))
      .update(IExtensible, AbsIValue(AT))
  }

  def newArgObject(absLength: AbsNum = AbsNum(0)): Elem = {
    Empty
      .update(IClass, AbsIValue(AbsStr("Arguments")))
      .update(IPrototype, AbsIValue(BuiltinObjectProto.loc))
      .update(IExtensible, AbsIValue(AT))
      .update("length", AbsDataProp(absLength, AT, AF, AT))
  }

  def newArrayObject(absLength: AbsNum = AbsNum(0)): Elem = {
    Empty
      .update(IClass, AbsIValue(AbsStr("Array")))
      .update(IPrototype, AbsIValue(BuiltinArrayProto.loc))
      .update(IExtensible, AbsIValue(AT))
      .update("length", AbsDataProp(absLength, AT, AF, AF))
  }

  def newFunctionObject(fid: FunctionId, env: AbsValue, l: Loc, n: AbsNum): Elem = {
    newFunctionObject(Some(fid), Some(fid), env, Some(l), n)
  }

  def newFunctionObject(fidOpt: Option[FunctionId], constructIdOpt: Option[FunctionId], env: AbsValue,
    locOpt: Option[Loc], n: AbsNum): Elem = {
    newFunctionObject(fidOpt, constructIdOpt, env,
      locOpt, AT, AF, AF, n)
  }

  def newFunctionObject(fidOpt: Option[FunctionId], constructIdOpt: Option[FunctionId], env: AbsValue,
    locOpt: Option[Loc], writable: AbsBool, enumerable: AbsBool, configurable: AbsBool,
    absLength: AbsNum): Elem = {
    val obj1 =
      Empty
        .update(IClass, AbsIValue(AbsStr("Function")))
        .update(IPrototype, AbsIValue(BuiltinFunctionProto.loc))
        .update(IExtensible, AbsIValue(AT))
        .update(IScope, AbsIValue(env))
        .update("length", AbsDataProp(absLength, AF, AF, AF))

    val obj2 = fidOpt match {
      case Some(fid) => obj1.update(ICall, AbsFId(fid))
      case None => obj1
    }
    val obj3 = constructIdOpt match {
      case Some(cid) => obj2.update(IConstruct, AbsFId(cid))
      case None => obj2
    }
    val obj4 = locOpt match {
      case Some(loc) =>
        val prototypeVal = AbsValue(loc)
        obj3.update(IHasInstance, AbsIValue(AbsNull.Top))
          .update("prototype", AbsDataProp(prototypeVal, writable, enumerable, configurable))
      case None => obj3
    }
    obj4
  }

  def newBooleanObj(absB: AbsBool): Elem = {
    val newObj = newObject(BuiltinBooleanProto.loc)
    newObj.update(IClass, AbsIValue(AbsStr("Boolean")))
      .update(IPrimitiveValue, AbsIValue(absB))
  }

  def newNumberObj(absNum: AbsNum): Elem = {
    val newObj = newObject(BuiltinNumberProto.loc)
    newObj.update(IClass, AbsIValue(AbsStr("Number")))
      .update(IPrimitiveValue, AbsIValue(absNum))
  }

  def newStringObj(absStr: AbsStr): Elem = {
    val newObj = newObject(BuiltinStringProto.loc)

    val newObj2 = newObj
      .update(IClass, AbsIValue(AbsStr("String")))
      .update(IPrimitiveValue, AbsIValue(absStr))

    absStr.gamma match {
      case ConFin(strSet) =>
        strSet.foldLeft(Bot)((obj, str) => {
          val length = str.length
          val newObj3 = (0 until length).foldLeft(newObj2)((tmpObj, tmpIdx) => {
            val charAbsStr = AbsStr(str.charAt(tmpIdx).toString)
            val charVal = AbsValue(charAbsStr)
            tmpObj.update(tmpIdx.toString, AbsDataProp(charVal, AF, AT, AF))
          })
          val lengthVal = AbsValue(length)
          obj ⊔ newObj3.update("length", AbsDataProp(lengthVal, AF, AF, AF))
        })
      case _ =>
        newObj2
          .weakUpdate(AbsStr.Number, AbsDataProp(AbsValue(AbsStr.Top), AF, AT, AF))
          .update("length", AbsDataProp(absStr.length, AF, AF, AF))
    }
  }

  def newErrorObj(errorName: String, protoLoc: Loc): Elem = {
    Empty
      .update(IClass, AbsIValue(AbsStr(errorName)))
      .update(IPrototype, AbsIValue(protoLoc))
      .update(IExtensible, AbsIValue(AbsBool.True))
  }

  def defaultValue(locSet: AbsLoc): AbsPValue = {
    if (locSet.isBottom) AbsPValue.Bot
    else AbsPValue.Top
  }

  def defaultValue(locSet: AbsLoc, preferredType: String): AbsPValue = {
    if (locSet.isBottom) AbsPValue.Bot
    else {
      preferredType match {
        case "Number" => AbsPValue(numval = AbsNum.Top)
        case "String" => AbsPValue(strval = AbsStr.Top)
        case _ => AbsPValue.Top
      }
    }
  }

  def defaultValue(locSet: AbsLoc, h: AbsHeap, preferredType: String): AbsPValue = {
    if (locSet.isBottom) AbsPValue.Bot
    else locSet.foldLeft(AbsPValue.Bot)((pv, loc) => h.get(loc).DefaultValue(preferredType, h))
  }

  // 8.10.4 FromPropertyDescriptor ( Desc )
  def FromPropertyDescriptor(h: AbsHeap, desc: AbsDesc): (Elem, Set[Exception]) = {
    def put(
      obj: Elem,
      name: String,
      pair: (AbsValue, AbsAbsent)
    ): (Elem, AbsBool, Set[Exception]) = {
      val T = (AbsBool.True, AbsAbsent.Bot)
      obj.DefineOwnProperty(
        AbsStr(name),
        AbsDesc(pair, T, T, T),
        Throw = false,
        h
      )
    }
    def toValue(pair: (AbsBool, AbsAbsent)): (AbsValue, AbsAbsent) = {
      val (b, a) = pair
      (AbsValue(b), a)
    }
    val (obj1, _, excSet1) = put(newObject, "value", desc.value)
    val (obj2, _, excSet2) = put(obj1, "writable", toValue(desc.writable))
    val (obj3, _, excSet3) = put(obj2, "enumerable", toValue(desc.enumerable))
    val (obj4, _, excSet4) = put(obj3, "configurable", toValue(desc.configurable))
    (obj4, excSet1 ++ excSet2 ++ excSet3 ++ excSet4)
  }
}
