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

// CORE States
case class State(
    insts: List[Inst] = Nil,
    env: Env = Env(),
    heap: Heap = Heap()
) {
  // update identifiers
  def update(id: Id, value: Value): State =
    copy(env = env.update(id, value))

  // update labels
  def update(label: Label, cont: LabelCont): State =
    copy(env = env.update(label, cont))

  // lookup identifiers
  def apply(id: Id): Value = env(id)

  // lookup labels
  def apply(label: Label): LabelCont = env(label)

  // new address
  def newAddr: Addr = heap.newAddr

  // addition
  def update(addr: Addr, obj: Obj): State =
    copy(heap = heap.update(addr, obj))

  // lookup addresses
  def apply(addr: Addr): Obj = heap(addr)
}
