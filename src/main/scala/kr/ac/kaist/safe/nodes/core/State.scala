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

case class State(env: Env = Env(), heap: Heap = Heap())

case class Env(map: Map[String, Value] = Map()) {
  // addition
  def +(pair: (Id, Value)): Env = Env(map + pair)

  // lookup variables
  def lookup(id: Id): Value =
    map.getOrElse(id, error(s"free identifier: $id"))
}
case class Heap(map: Map[Addr, Obj] = Map()) {
  // new address
  def malloc: Addr = Addr(map.size)

  // addition
  def +(pair: (Addr, Obj)): Heap = Heap(map + pair)

  // lookup addresses
  def lookup(addr: Addr): Obj =
    map.getOrElse(addr, error(s"free address: $addr"))
}

case class Obj(map: Map[Str, Value] = Map()) {
  // lookup properties
  def lookup(prop: Str): Value =
    map.getOrElse(prop, error(s"free property: $prop"))

  // addition
  def +(pair: (Str, Value)): Obj = Obj(map + pair)

  // deletion
  def -(prop: Str): Obj = Obj(map - prop)
}
