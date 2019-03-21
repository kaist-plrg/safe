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

// CORE Heaps
case class Heap(
    map: Map[Addr, Obj] = Map(),
    size: Int = 0
) {
  // new addresses
  def alloc: (Addr, Heap) = {
    val addr = DynAddr(size)
    (addr, Heap(map + (addr -> Obj()), size + 1))
  }

  // update objects
  def update(addr: Addr, obj: Obj): Heap = Heap(map + (addr -> obj))

  // lookup addresses
  def apply(addr: Addr): Obj =
    map.getOrElse(addr, error(s"free address: $addr"))

  def appendTo(
    sb: StringBuilder,
    indent: String = "",
    firstIndent: Boolean = true,
    detail: Boolean = true
  ): StringBuilder = (sb /: map) {
    case (sb, (StaAddr(name), obj)) if !detail => sb
    case (sb, (addr, obj)) =>
      sb.append(indent)
      addr.appendTo(sb).append(" -> {").append(LINE_SEP)
      obj.appendTo(sb, indent + TAB, firstIndent, detail)
      sb.append(indent).append("}").append(LINE_SEP)
  }
}
object Heap {
  def apply(seq: (Addr, Obj)*): Heap = Heap(Map(seq: _*))
}
