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
    globals: Map[Id, Value] = Map(),
    env: Env = Env(),
    heap: Heap = Heap()
) {
  def getId(id: Id): Value =
    env.locals.getOrElse(id, globals.getOrElse(id, error(s"free identifier: $id")))

  def appendTo(
    sb: StringBuilder,
    indent: String = "",
    firstIndent: Boolean = true,
    detail: Boolean = true
  ): StringBuilder = {
    val newIndent = indent + TAB
    sb.append(indent).append("* global identifiers: ")
    if (detail) {
      sb.append(LINE_SEP)
      appendMap(sb, globals, newIndent + TAB, detail)
    } else sb.append("...").append(LINE_SEP)
    sb.append(LINE_SEP)

    sb.append("* environment:").append(LINE_SEP)
    env.appendTo(sb, newIndent, false, detail)
    sb.append("* heap:").append(LINE_SEP)
    heap.appendTo(sb, newIndent + TAB, false, detail).append(LINE_SEP)
    sb.append("* instructions:").append(LINE_SEP)
    (sb /: insts) {
      case (sb, inst) => inst.appendTo(sb, newIndent + TAB).append(LINE_SEP)
    }
  }
}
