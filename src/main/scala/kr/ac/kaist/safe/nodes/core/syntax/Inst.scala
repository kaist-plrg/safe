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

// CORE Instructions
sealed trait Inst extends CoreNode
case class IExpr(ref: Ref, expr: Expr) extends Inst
case class IAlloc(ref: Ref, ty: Ty) extends Inst
case class IDelete(ref: Ref) extends Inst
case class IApp(ref: Ref, fexpr: Expr, args: List[Expr]) extends Inst
case class IReturn(expr: Expr) extends Inst
case class IIf(cond: Expr, thenInst: Inst, elseInst: Inst) extends Inst
case class IWhile(cond: Expr, body: Inst) extends Inst
case class ITry(ref: Ref, tryInst: Inst) extends Inst
case class IThrow(expr: Expr) extends Inst
case class ISeq(insts: List[Inst]) extends Inst
case class IAssert(expr: Expr) extends Inst
case class IPrint(expr: Expr) extends Inst
case object INotYetImpl extends Inst
