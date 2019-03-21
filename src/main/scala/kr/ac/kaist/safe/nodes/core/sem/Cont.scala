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

// CORE Continuations
trait Cont extends Appendable {
  def appendTo(
    sb: StringBuilder,
    indent: String = "",
    firstIndent: Boolean = true,
    detail: Boolean = true
  ): StringBuilder = {
    if (firstIndent) sb.append(indent)
    val newIndent = indent + TAB
    def appendInsts(insts: List[Inst]): StringBuilder = {
      sb.append(LINE_SEP)
      (sb /: insts) {
        case (sb, inst) => inst.appendTo(sb, newIndent, true).append(LINE_SEP)
      }
    }
    this match {
      case LabelCont(insts) =>
        if (detail) appendInsts(insts)
        else sb.append("...")
      case ScopeCont(id, insts, env) =>
        sb.append(indent).append("id: ").append(id).append(LINE_SEP)
        sb.append(indent).append("insts: ")
        if (detail) appendInsts(insts)
        else sb.append("...").append(LINE_SEP)
        sb.append(indent).append("environment: ")
        if (detail) {
          sb.append(LINE_SEP)
          env.appendTo(sb, newIndent, true, detail)
        } else sb.append("...").append(LINE_SEP)
    }
  }
}
case class LabelCont(insts: List[Inst]) extends Cont
case class ScopeCont(id: Id, insts: List[Inst], env: Env) extends Cont
