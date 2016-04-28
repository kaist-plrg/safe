/**
 * *****************************************************************************
 * Copyright (c) 2016, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.errors.error

import kr.ac.kaist.safe.nodes.{ Node, IRId, Id, Label, VarDecl, UnaryAssignOpApp, InfixOpApp }

sealed abstract class CompileError(msg: String, node: Node) extends SafeError({
  s"${node.info.span}: $msg"
})

case class IRIdNotBoundError(name: String, id: IRId) extends CompileError({
  s"Identifier $name is not bound."
}, id)
case class NotUniqueIdError(id: Id) extends CompileError({
  s"Identifiers should have a unique name after the disambiguation phase: ${id.text}"
}, id)
case class NotUniqueLabelError(l: Label) extends CompileError({
  s"Labels should have a unique name after the disambiguation phase: ${l.id.text}"
}, l)
case class VarDeclNotHaveInitExprError(vd: VarDecl) extends CompileError({
  "Variable declarations should not have any initialization expressions after the disambiguation phase."
}, vd)
case class InvalidUnAssignOpError(u: UnaryAssignOpApp) extends CompileError({
  s"Invalid UnaryAssignOpApp operator: ${u.op.text}"
}, u)
case class InvalidInfixOpAppError(infix: InfixOpApp) extends CompileError({
  s"Infix operator ${infix.op.text} should have at least two arguments"
}, infix)