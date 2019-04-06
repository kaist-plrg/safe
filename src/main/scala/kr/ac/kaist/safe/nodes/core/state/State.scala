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

// CORE States
case class State(
    insts: List[Inst],
    globals: Map[Id, Value],
    env: Env,
    heap: Heap
) extends CoreNode {
  // existence check
  def contains(base: Base): Boolean = base match {
    case BaseId(id) =>
      heap.contains(env.locals, id) || globals.contains(id)
    case BaseIdProp(addr, id) =>
      heap.contains(addr, id)
    case BaseStrProp(addr, str) =>
      heap.contains(addr, str)
  }

  // getters
  def apply(base: Base): Value = base match {
    case BaseId(id) =>
      if (heap.contains(env.locals, id)) heap(env.locals, id)
      else globals.getOrElse(id, error(s"free identifiers: ${beautify(id)}"))
    case BaseIdProp(addr, id) =>
      heap(addr, id)
    case BaseStrProp(addr, str) =>
      heap(addr, str)
  }

  // setters
  def updated(base: Base, value: Value): State = copy(heap = base match {
    case BaseId(id) =>
      heap.updated(env.locals, id, value)
    case BaseIdProp(addr, id) =>
      heap.updated(addr, id, value)
    case BaseStrProp(addr, str) =>
      heap.updated(addr, str, value)
  })

  // deletes
  def deleted(base: Base): State = copy(heap = base match {
    case BaseId(id) =>
      heap.deleted(env.locals, id)
    case BaseIdProp(addr, id) =>
      heap.deleted(addr, id)
    case BaseStrProp(addr, str) =>
      heap.deleted(addr, str)
  })

  // object allocations
  def alloc(ty: Ty): (Addr, State) = alloc(ty, Map(), Map())
  def alloc(ty: Ty, idMap: Map[Id, Value], strMap: Map[String, Value]): (Addr, State) = {
    val (newAddr, newHeap) = heap.alloc(ty, idMap, strMap)
    (newAddr, copy(heap = newHeap))
  }

  // environment allocations
  def allocLocals(idMap: Map[Id, Value]): (Addr, State) = {
    val (newAddr, newHeap) = heap.allocLocals(idMap)
    (newAddr, copy(heap = newHeap))
  }

  // continue with continuations
  def continue(cont: Cont, value: Value): State = {
    val retSt = copy(insts = cont.insts, env = cont.env)
    retSt.updated(cont.base, value)
  }
}
