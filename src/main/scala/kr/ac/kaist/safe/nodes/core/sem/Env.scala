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

// CORE Environments
case class Env(
    locals: Map[Id, Value] = Map(),
    labels: Map[Label, LabelCont] = Map(),
    retLabel: Option[ScopeCont] = None,
    excLabel: Option[ScopeCont] = None
) {
  // update identifiers
  def setId(id: Id, value: Value): Env =
    copy(locals = locals + (id -> value))

  // update labels
  def setLabel(label: Label, cont: LabelCont): Env =
    copy(labels = labels + (label -> cont))

  // update label maps
  def setLabelMap(labels: Map[Label, LabelCont]): Env =
    copy(labels = labels)

  // update return label
  def setRetLabel(cont: ScopeCont): Env =
    copy(retLabel = Some(cont))

  // update exception label
  def setExcLabel(cont: ScopeCont): Env =
    copy(excLabel = Some(cont))

  // lookup labels
  def getLabel(label: Label): LabelCont =
    labels.getOrElse(label, error(s"free label: $label"))

  def appendTo(
    sb: StringBuilder,
    indent: String = "",
    firstIndent: Boolean = true,
    detail: Boolean = true
  ): StringBuilder = {
    val newIndent = indent + TAB + TAB
    sb.append(indent).append("- local identifiers:").append(LINE_SEP)
    appendMap(sb, locals, newIndent, detail)
    sb.append(LINE_SEP)

    sb.append(indent).append("- labels:").append(LINE_SEP)
    (sb /: labels) {
      case (sb, (k, v)) =>
        sb.append(newIndent).append(k).append(" -> ").append(LINE_SEP)
        v.appendTo(sb, newIndent + TAB, false, detail)
    }
    sb.append(LINE_SEP)

    sb.append(indent).append("- return: ")
    appendOpt(sb, retLabel, newIndent, detail)
    sb.append(LINE_SEP)

    sb.append(indent).append("- exception: ")
    appendOpt(sb, excLabel, newIndent, detail)
    sb.append(LINE_SEP)
  }
}
