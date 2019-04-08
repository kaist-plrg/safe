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
import kr.ac.kaist.safe.nodes.ast._
import kr.ac.kaist.safe.nodes.core
import kr.ac.kaist.safe.util.Useful.treeString

/* Translates JavaScript AST to IR. */
class JSLoader(program: Program, model: core.Model) {
  ////////////////////////////////////////////////////////////////
  // Results
  ////////////////////////////////////////////////////////////////
  lazy val result: core.State = {
    val programId = core.Id("program")
    val resultId = core.Id("result")
    val callInst = core.IApp(
      core.LhsLet(resultId),
      core.ERef(core.RefIdProp(core.RefId(programId), evalId)),
      List(core.ERef(core.RefId(programId)))
    )
    st = model.getInitial(core.Program(List(callInst)))

    Walker.walk(program)
    st.updated(core.GlobalId(programId), last)
  }

  ////////////////////////////////////////////////////////////////
  // Private Mutable Variables
  ////////////////////////////////////////////////////////////////
  private var st: core.State = null
  private var last: core.Addr = null
  private var decls: Decls = new Decls(Nil, Nil)

  class Decls(var fds: List[core.Addr], var vds: List[core.Addr])

  ////////////////////////////////////////////////////////////////
  // Translations
  ////////////////////////////////////////////////////////////////
  private val evalId = core.Id("eval")
  private val typeId = core.Id("type")
  private val fdsId = core.Id("fds")
  private val vdsId = core.Id("vds")
  private val ProgramId = core.Id("Program")
  private val SourceElementsId = core.Id("SourceElements")
  private val SourceElementId = core.Id("SourceElement")
  private val StatementId = core.Id("Statement")
  private val FunctionDeclarationId = core.Id("FunctionDeclaration")
  private val IdentifierId = core.Id("Identifier")
  private val VariableStatementId = core.Id("VariableStatement")
  private val VariableDeclarationListId = core.Id("VariableDeclarationList")
  private val VariableDeclarationId = core.Id("VariableDeclaration")
  private val InitialiserId = core.Id("Initialiser")
  private val AssignmentExpressionId = core.Id("AssignmentExpression")
  private val ConditionalExpressionId = core.Id("ConditionalExpression")
  private val LogicalORExpressionId = core.Id("LogicalORExpression")
  private val LogicalANDExpressionId = core.Id("LogicalANDExpression")
  private val BitwiseORExpressionId = core.Id("BitwiseORExpression")
  private val BitwiseXORExpressionId = core.Id("BitwiseXORExpression")
  private val BitwiseANDExpressionId = core.Id("BitwiseANDExpression")
  private val EqualityExpressionId = core.Id("EqualityExpression")
  private val RelationalExpressionId = core.Id("RelationalExpression")
  private val ShiftExpressionId = core.Id("ShiftExpression")
  private val AdditiveExpressionId = core.Id("AdditiveExpression")
  private val MultiplicativeExpressionId = core.Id("MultiplicativeExpression")
  private val UnaryExpressionId = core.Id("UnaryExpression")
  private val PostfixExpressionId = core.Id("PostfixExpression")
  private val LeftHandSideExpressionId = core.Id("LeftHandSideExpression")
  private val NewExpressionId = core.Id("NewExpression")
  private val MemberExpressionId = core.Id("MemberExpression")
  private val PrimaryExpressionId = core.Id("PrimaryExpression")
  private val LiteralId = core.Id("Literal")
  private val NumericLiteral = core.Id("NumericLiteral")

  private object Walker extends ASTUnitWalker {
    override def walk(node: Program): Unit = node match {
      case Program(_, TopLevel(_, _, _, stmts)) =>
        val addr = create("Program")

        stmts.foreach(walk(_))
        st = st.updated(addr, SourceElementsId, last)

        createList(decls.fds)
        st = st.updated(addr, fdsId, last)

        createList(decls.vds)
        st = st.updated(addr, vdsId, last)

        st = st.updated(addr, evalId, st.globals(ProgramId))

        last = addr
    }

    override def walk(node: FunDecl): Unit = node match {
      case fd @ FunDecl(_, ftn, isStrict) =>
        val addr = create("FunctionDeclaration")

        val oldDecls = decls
        decls = new Decls(Nil, Nil)

        createList(decls.fds)
        st = st.updated(addr, fdsId, last)

        createList(decls.vds)
        st = st.updated(addr, vdsId, last)
    }

    def create(tyName: String): core.Addr = {
      val (addr, newSt) = st.alloc(core.Ty(tyName))
      st = newSt
      addr
    }

    def createList(vs: List[core.Value]): Unit = {
      ???
    }
  }

  ////////////////////////////////////////////////////////////////
  // Helper Functions
  ////////////////////////////////////////////////////////////////
  // XXX temporal debugging tool
  private def todo(x: Any): Nothing = {
    println(x)
    ???
  }
}
