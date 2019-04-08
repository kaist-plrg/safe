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

package kr.ac.kaist.safe.compiler

import kr.ac.kaist.safe.nodes.ast._

class DeclCollector extends ASTWalker {
  var fds: List[FunDecl] = Nil
  var vds: List[VarDecl] = Nil

  override def walk(node: TopLevel): TopLevel = node match {
    case TopLevel(info, _, _, stmts) =>
      val newStmts = stmts.map(walk)
      TopLevel(walk(info), fds.reverse, vds.reverse, newStmts)
  }

  override def walk(node: FunDecl): FunDecl = node match {
    case fd @ FunDecl(info, ftn, isStrict) =>
      val collector = new DeclCollector
      val newFd = FunDecl(walk(info), collector.walk(ftn), isStrict)
      fds ::= newFd
      newFd
  }

  override def walk(node: Functional): Functional = node match {
    case Functional(info, _, _, stmts, name, params, body) =>
      val newStmts = walk(stmts)
      Functional(
        walk(info),
        fds.reverse,
        vds.reverse,
        newStmts,
        walk(name),
        params.map(walk),
        body
      )
  }

  override def walk(node: VarDecl): VarDecl = node match {
    case vd @ VarDecl(info, name, expr, isStrict) =>
      vds ::= vd
      VarDecl(walk(info), walk(name), expr.map(walk), isStrict)
  }
}
