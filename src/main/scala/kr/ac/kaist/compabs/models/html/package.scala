/**
 * *****************************************************************************
 * Copyright (c) 2015-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models

import kr.ac.kaist.safe.analyzer.domain.{ AbsLexEnv, AbsState, AbsValue }
import kr.ac.kaist.safe.analyzer.models.{ SemanticFun }
import kr.ac.kaist.safe.nodes.ast.{ ASTNodeInfo, ModelFunc }
import kr.ac.kaist.safe.nodes.cfg.{ CFG, CFGEdgeExc, CFGFunction, ModelBlock }
import kr.ac.kaist.safe.nodes.ir.IRModelFunc
import kr.ac.kaist.safe.util.{ AllocSite, Span }

import scala.collection.immutable.{ HashMap, HashSet }

package object html {
  case class CodeFragment(filename: String, startLine: Int, startOffset: Int, content: String)
  type Handlers = HashMap[String, CodeFragment]

  object EventTypeEnum extends Enumeration {
    type EventType = Value
    val Load, Unload, Keyboard, Mouse, Message, ReadyStateChange, DOMLoaded, Timer, Focus, Other = Value
  }

  class ModelCode(
      argLen: Int = 0,
      asiteSet: Set[AllocSite] = HashSet(),
      code: (AbsValue, AbsState) => (AbsState, AbsState, AbsValue)
  ) {
    private def createCFGFunc(cfg: CFG, name: String): (String, String, CFGFunction) = {
      val funName: String = s"[]$name"
      val argsName: String = s"<>arguments<>$funName"
      val ir: IRModelFunc = IRModelFunc(ModelFunc(ASTNodeInfo(Span(funName))))
      val func: CFGFunction = CFGFunction(ir, argsName, List.empty, List.empty, name, false)
      (funName, argsName, func)
    }

    def getCFGFunc(cfg: CFG, name: String): CFGFunction = {
      val (funName, argsName, func) = createCFGFunc(cfg, name)
      val sem: SemanticFun = createSemanticFunc(argsName)
      val modelBlock: ModelBlock = func.createModelBlock(sem)
      cfg.addEdge(func.entry, modelBlock)
      cfg.addEdge(modelBlock, func.exit)
      cfg.addEdge(modelBlock, func.exitExc, CFGEdgeExc)
      func
    }

    private def createSemanticFunc(argsName: String): SemanticFun = st => {
      val heap = st.heap
      val context = st.context
      val stBotPair = (AbsState.Bot, AbsState.Bot)
      val localEnv = context.pureLocal.record.decEnvRec
      val (argV, _) = localEnv.GetBindingValue(argsName)
      val (retSt, retSte, retV) = code(argV, st)
      val (retObj, _) = localEnv.SetMutableBinding("@return", retV)
      val retCtx = retSt.context.subsPureLocal(AbsLexEnv(retObj))
      (retSt.copy(context = retCtx), retSte)
    }
  }

  object ModelCode {
    def apply(
      argLen: Int = 0,
      asiteSet: Set[AllocSite] = HashSet(),
      code: (AbsValue, AbsState) => (AbsState, AbsState, AbsValue)
    ): ModelCode = new ModelCode(argLen, asiteSet, code)
  }
}
