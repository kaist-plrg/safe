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

package kr.ac.kaist.safe.nodes.cfg

import kr.ac.kaist.safe.util.NodeUtil

sealed abstract class CFGId(
    val text: String,
    val kind: VarKind,
    val keyVar: Boolean = false,
    val headID: Option[BlockId] = None
) {
  def makeKey(head: Option[BlockId]): CFGId
}

case class CFGUserId(
    override val text: String,
    override val kind: VarKind,
    originalName: String,
    fromWith: Boolean,
    override val keyVar: Boolean = false,
    override val headID: Option[BlockId] = None
) extends CFGId(text, kind) {
  override def makeKey(head: Option[BlockId]): CFGUserId = copy(keyVar = true, headID = head)
  override def toString: String = {
    val post =
      if (keyVar) "(*)"
      else ""
    NodeUtil.pp(text) + post
  }
}

case class CFGTempId(
    override val text: String,
    override val kind: VarKind,
    override val keyVar: Boolean = false,
    override val headID: Option[BlockId] = None
) extends CFGId(text, kind) {
  override def makeKey(head: Option[BlockId]): CFGTempId = copy(keyVar = true, headID = head)
  override def toString: String = {
    val post =
      if (keyVar) "(*)"
      else ""
    NodeUtil.pp(text) + post
  }
}

sealed abstract class VarKind
case object GlobalVar extends VarKind
case object PureLocalVar extends VarKind
case object CapturedVar extends VarKind
case object CapturedCatchVar extends VarKind
