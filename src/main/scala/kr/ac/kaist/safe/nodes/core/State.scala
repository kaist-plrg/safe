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

import kr.ac.kaist.safe.LINE_SEP

case class State(env: Env = Env(), heap: Heap = Heap()) {
  // conversion to string
  def stringTo(sb: StringBuilder): StringBuilder = {
    sb.append("Env: ")
    env.stringTo(sb).append(LINE_SEP)
    sb.append("Heap: ")
    heap.stringTo(sb)
  }
  override def toString: String = stringTo(new StringBuilder).toString
}

case class Env(map: Map[String, Value] = Map()) {
  // addition
  def +(pair: (Id, Value)): Env = Env(map + pair)

  // lookup variables
  def lookup(id: Id): Value =
    map.getOrElse(id, error(s"free identifier: $id"))

  // conversion to string
  def stringTo(sb: StringBuilder): StringBuilder = {
    sb.append("{").append(LINE_SEP)
    map.foreach {
      case (x, v) => sb.append(TAB).append(s"$x -> $v").append(LINE_SEP)
    }
    sb.append("}")
  }
  override def toString: String = stringTo(new StringBuilder).toString
}
case class Heap(map: Map[Addr, Obj] = Map()) {
  // new address
  def malloc: Addr = Addr(map.size)

  // addition
  def +(pair: (Addr, Obj)): Heap = Heap(map + pair)

  // lookup addresses
  def lookup(addr: Addr): Obj =
    map.getOrElse(addr, error(s"free address: $addr"))

  // conversion to string
  def stringTo(sb: StringBuilder): StringBuilder = {
    sb.append("{").append(LINE_SEP)
    map.foreach {
      case (a, o) =>
        sb.append(TAB).append(s"$a -> ")
        o.stringTo(sb, TAB, true).append(LINE_SEP)
    }
    sb.append("}")
  }
  override def toString: String = stringTo(new StringBuilder).toString
}

case class Obj(map: Map[Str, Value] = Map()) {
  // lookup properties
  def lookup(prop: Str): Value =
    map.getOrElse(prop, error(s"free property: $prop"))

  // addition
  def +(pair: (Str, Value)): Obj = Obj(map + pair)

  // deletion
  def -(prop: Str): Obj = Obj(map - prop)

  // conversion to string
  def stringTo(
    sb: StringBuilder,
    indent: String = "",
    body: Boolean = false
  ): StringBuilder = {
    if (!body) sb.append(TAB)
    val prefix = indent + TAB
    sb.append("{").append(LINE_SEP)
    map.foreach {
      case (p, v) => sb.append(prefix).append("$p -> $v").append(LINE_SEP)
    }
    sb.append("}")
  }
  override def toString: String = stringTo(new StringBuilder).toString
}
