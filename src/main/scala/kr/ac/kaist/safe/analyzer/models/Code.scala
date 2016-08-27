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

package kr.ac.kaist.safe.analyzer.models

import kr.ac.kaist.safe.analyzer.Semantics
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.models.PredefLoc.SINGLE_PURE_LOCAL
import kr.ac.kaist.safe.nodes.cfg.{ CFG, CFGFunction, ModelBlock, CFGEdgeExc }
import kr.ac.kaist.safe.nodes.ir.IRModelFunc
import kr.ac.kaist.safe.nodes.ast.{ ModelFunc, ASTNodeInfo }
import kr.ac.kaist.safe.util.Span

abstract class Code {
  val argLen: Int = 0
  def getCFGFunc(cfg: CFG, name: String, utils: Utils): CFGFunction
  protected def createCFGFunc(
    cfg: CFG,
    name: String,
    utils: Utils
  ): (String, String, CFGFunction) = {
    val funName: String = s"[]$name"
    val argsName: String = s"<>arguments<>$funName"
    val ir: IRModelFunc = IRModelFunc(ModelFunc(ASTNodeInfo(Span(funName))))
    val func: CFGFunction =
      cfg.createFunction(argsName, Nil, Nil, funName, ir, "", false)
    (funName, argsName, func)
  }
}

class EmptyCode(
    override val argLen: Int = 0
) extends Code {
  def getCFGFunc(cfg: CFG, name: String, utils: Utils): CFGFunction = {
    val (_, _, func) = createCFGFunc(cfg, name, utils)
    cfg.addEdge(func.entry, func.exit)
    func
  }
}
object EmptyCode {
  def apply(argLen: Int = 0): EmptyCode = new EmptyCode(argLen)
}

class BasicCode(
    override val argLen: Int = 0,
    code: (Value, State, Semantics, Utils) => (State, State, Value)
) extends Code {
  def getCFGFunc(cfg: CFG, name: String, utils: Utils): CFGFunction = {
    val (funName, argsName, func) = createCFGFunc(cfg, name, utils)
    val sem: SemanticFun = createSemanticFunc(argsName, utils)
    val modelBlock: ModelBlock = func.createModelBlock(sem)
    cfg.addEdge(func.entry, modelBlock)
    cfg.addEdge(modelBlock, func.exit)
    cfg.addEdge(modelBlock, func.exitExc, CFGEdgeExc)
    func
  }

  private def createSemanticFunc(argsName: String, utils: Utils): SemanticFun = (sem, st) => st match {
    case State(heap, ctx) => {
      val stBotPair = (State.Bot, State.Bot)
      heap(SINGLE_PURE_LOCAL) match {
        case Some(localObj) => {
          localObj(argsName) match {
            case Some(pv) => {
              val (retSt, retSte, retV) = code(pv.objval.value, st, sem, utils)
              val retObj = localObj.update("@return", PropValue(utils.dataProp(retV)))
              val retHeap = retSt.heap.update(SINGLE_PURE_LOCAL, retObj)
              (State(retHeap, st.context), retSte)
            }
            case None => stBotPair // TODO dead code
          }
        }
        case None => stBotPair // TODO dead code
      }
    }
  }
}
object BasicCode {
  def apply(
    argLen: Int = 0,
    code: (Value, State, Semantics, Utils) => (State, State, Value)
  ): BasicCode = new BasicCode(argLen, code)
}

class SimpleCode(
  override val argLen: Int = 0,
  code: (Value, Heap, Semantics, Utils) => Value
) extends BasicCode(argLen, (v: Value, st: State, sem: Semantics, utils: Utils) => {
  (st, State.Bot, code(v, st.heap, sem, utils))
})
object SimpleCode {
  def apply(
    argLen: Int = 0,
    code: (Value, Heap, Semantics, Utils) => Value
  ): SimpleCode = new SimpleCode(argLen, code)
}