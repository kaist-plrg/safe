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

import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.nodes.ast
import kr.ac.kaist.safe.nodes.core
import scala.collection.mutable.MutableList

/* Translates JavaScript AST to IR. */
class Compiler(program: ast.Program) {
  ////////////////////////////////////////////////////////////////
  // Results
  ////////////////////////////////////////////////////////////////
  lazy val result: core.Program = {
    val insts = MutableList[core.Inst]()
    walkProgram(program, insts)
    core.Program(insts.toList)
  }

  ////////////////////////////////////////////////////////////////
  // Private Mutable Variables
  ////////////////////////////////////////////////////////////////
  private var idCount: Long = 0
  private var env: Map[ast.Id, String] = Map()

  ////////////////////////////////////////////////////////////////
  // Translations
  ////////////////////////////////////////////////////////////////
  // for Program
  private def walkProgram(
    pgm: ast.Program,
    insts: MutableList[core.Inst]
  ): Unit = {
    val ast.Program(_, ast.TopLevel(_, fds, vds, stmts)) = pgm
    fds.foreach(walkFd(_, insts))
    vds.foreach(walkVd(_, insts))
    stmts.foreach(_.body.foreach(walkStmt(_, insts)))
  }

  // for FunDecl
  private def walkFd(
    fd: ast.FunDecl,
    insts: MutableList[core.Inst]
  ): Unit = {
    val ast.FunDecl(_, f, _) = fd
    walkFunctional(f, insts)
  }

  // for Functional
  private def walkFunctional(
    functional: ast.Functional,
    insts: MutableList[core.Inst]
  ): Unit = todo(functional)

  // for VarDecl
  private def walkVd(
    vd: ast.VarDecl,
    insts: MutableList[core.Inst]
  ): Unit = todo(vd)

  // for Stmt
  private def walkStmt(
    stmt: ast.Stmt,
    insts: MutableList[core.Inst]
  ): Unit = stmt match {
    case (_: ast.NoOp) =>
    // 12.2 Variable Statement
    case ast.VarStmt(_, vds) => vds.foreach(walkVd(_, insts))
    case s => todo(s)
  }

  // for Expr
  private def walkExpr(
    expr: ast.Expr,
    insts: MutableList[core.Inst]
  ): String = todo(expr)

  ////////////////////////////////////////////////////////////////
  // Helper Functions
  ////////////////////////////////////////////////////////////////
  // create a new id
  private def newId(id: ast.Id): String = {
    val nid = newId
    env += (id -> newId)
    nid
  }
  private def newId: String = {
    val id = s"x$idCount"
    idCount += 1
    id
  }

  // XXX temporal debugging tool
  private def todo(x: Any): Nothing = {
    println(x)
    ???
  }
}
