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

package kr.ac.kaist.safe.analyzer

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.util.NodeUtil

import scala.collection.immutable.HashSet

case class Initialize(helper: Helper) {
  val utils = helper.utils
  val addrManager = helper.addrManager

  private def initObj(protoLoc: Loc, className: String): Obj = {
    val absFalse = utils.absBool.False
    val protoVal = Value(utils.PValueBot, HashSet(protoLoc))
    utils.ObjEmpty
      .update("@class", PropValue(utils.ObjectValueBot.copyWith(utils.absString.alpha(className))))
      .update("@proto", PropValue(ObjectValue(protoVal, absFalse, absFalse, absFalse)))
      .update("@extensible", PropValue(utils.ObjectValueBot.copyWith(utils.absBool.True)))
  }

  def state: State = {
    val globalPureLocalObj = helper.newPureLocal(Value(utils.PValueBot.copyWith(utils.absNull.Top)), HashSet(addrManager.PredefLoc.GLOBAL)) - "@return"

    val absFalse = utils.absBool.False
    val absTrue = utils.absBool.True
    val globalObjV = ObjectValue(Value(utils.PValueBot, HashSet(addrManager.PredefLoc.GLOBAL)), absFalse, absFalse, absFalse)
    val globalObj = helper.newObject(addrManager.ProtoLoc.OBJ)
      .update(NodeUtil.GLOBAL_NAME, PropValue(globalObjV))
      .update(NodeUtil.VAR_TRUE, PropValue(utils.ObjectValueBot.copyWith(utils.absBool.alpha(true))))
      .update("NaN", PropValue(utils.PValueBot.copyWith(utils.absNumber.NaN), absFalse, absFalse, absFalse))
      .update("Infinity", PropValue(utils.PValueBot.copyWith(utils.absNumber.PosInf), absFalse, absFalse, absFalse))
      .update("undefined", PropValue(utils.PValueBot.copyWith(utils.absUndef.Top), absFalse, absFalse, absFalse))
      .update("Object", PropValue(ObjectValue(Value(utils.PValueBot, HashSet(addrManager.ConstructorLoc.OBJ)), absTrue, absFalse, absTrue)))
      .update("Array", PropValue(ObjectValue(Value(utils.PValueBot, HashSet(addrManager.ConstructorLoc.ARRAY)), absTrue, absFalse, absTrue)))

    //TODO need modeling for initial values of Proto
    val protoObjProto = utils.ObjectValueBot.copyWith(utils.absNull.Top)
    val objPtoro = helper.newObject()
      .update("@proto", PropValue(protoObjProto))
      .update("constructor", PropValue(ObjectValue(Value(utils.PValueBot, HashSet(addrManager.ConstructorLoc.OBJ)), absTrue, absFalse, absTrue)))

    val functionProto = initObj(addrManager.ProtoLoc.FUNCTION, "Function")
    val arrayProto = initObj(addrManager.ProtoLoc.OBJ, "Array")
    val booleanProto = initObj(addrManager.ProtoLoc.OBJ, "Boolean")
    val numberProto = initObj(addrManager.ProtoLoc.OBJ, "Number")
    val stringProto = initObj(addrManager.ProtoLoc.OBJ, "String")

    val err = initObj(addrManager.ProtoLoc.ERR, "Error")
    val evalErr = initObj(addrManager.ProtoLoc.EVAL_ERR, "Error")
    val rangeErr = initObj(addrManager.ProtoLoc.RANGE_ERR, "Error")
    val refErr = initObj(addrManager.ProtoLoc.REF_ERR, "Error")
    val syntaxErr = initObj(addrManager.ProtoLoc.SYNTAX_ERR, "Error")
    val typeErr = initObj(addrManager.ProtoLoc.TYPE_ERR, "Error")
    val uriErr = initObj(addrManager.ProtoLoc.URI_ERR, "Error")

    val errProto = initObj(addrManager.ProtoLoc.OBJ, "Error")
      .update("name", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("Error")), absTrue, absFalse, absTrue))
      .update("message", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("")), absTrue, absFalse, absTrue))
    val evalErrProto = initObj(addrManager.ProtoLoc.ERR, "Error")
      .update("name", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("EvalError")), absTrue, absFalse, absTrue))
      .update("message", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("")), absTrue, absFalse, absTrue))
    val rangeErrProto = initObj(addrManager.ProtoLoc.ERR, "Error")
      .update("name", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("RangeError")), absTrue, absFalse, absTrue))
      .update("message", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("")), absTrue, absFalse, absTrue))
    val refErrProto = initObj(addrManager.ProtoLoc.ERR, "Error")
      .update("name", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("ReferenceError")), absTrue, absFalse, absTrue))
      .update("message", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("")), absTrue, absFalse, absTrue))
    val syntaxErrProto = initObj(addrManager.ProtoLoc.ERR, "Error")
      .update("name", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("SyntaxError")), absTrue, absFalse, absTrue))
      .update("message", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("")), absTrue, absFalse, absTrue))
    val typeErrProto = initObj(addrManager.ProtoLoc.ERR, "Error")
      .update("name", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("TypeError")), absTrue, absFalse, absTrue))
      .update("message", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("")), absTrue, absFalse, absTrue))
    val uriErrProto = initObj(addrManager.ProtoLoc.ERR, "Error")
      .update("name", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("URIError")), absTrue, absFalse, absTrue))
      .update("message", PropValue(utils.PValueBot.copyWith(utils.absString.alpha("")), absTrue, absFalse, absTrue))

    val objConstructor = initObj(addrManager.ProtoLoc.FUNCTION, "Function")
      .update("@scope", PropValue(utils.ObjectValueBot.copyWith(utils.absNull.Top)))
      .update("@hasinstance", PropValue(utils.ObjectValueBot.copyWith(utils.absNull.Top)))
      .update("prototype", PropValue(ObjectValue(Value(utils.PValueBot, HashSet(addrManager.ProtoLoc.OBJ)), absFalse, absFalse, absFalse)))
      .update("length", PropValue(utils.PValueBot.copyWith(utils.absNumber.alpha(1)), absFalse, absFalse, absFalse))

    val arrayConstructor = initObj(addrManager.ProtoLoc.OBJ, "Function")
      .update("@scope", PropValue(utils.ObjectValueBot.copyWith(utils.absNull.Top)))
      .update("@hasinstance", PropValue(utils.ObjectValueBot.copyWith(utils.absNull.Top)))
      .update("prototype", PropValue(ObjectValue(Value(utils.PValueBot, HashSet(addrManager.ProtoLoc.ARRAY)), absFalse, absFalse, absFalse)))
      .update("length", PropValue(utils.PValueBot.copyWith(utils.absNumber.alpha(1)), absFalse, absFalse, absFalse))

    val initHeap = Heap.Bot
      .update(addrManager.PredefLoc.SINGLE_PURE_LOCAL, globalPureLocalObj)
      .update(addrManager.PredefLoc.GLOBAL, globalObj)
      .update(addrManager.PredefLoc.COLLAPSED, utils.ObjEmpty)

      .update(addrManager.ProtoLoc.OBJ, objPtoro)
      .update(addrManager.ProtoLoc.FUNCTION, functionProto)
      .update(addrManager.ProtoLoc.ARRAY, arrayProto)
      .update(addrManager.ProtoLoc.BOOLEAN, booleanProto)
      .update(addrManager.ProtoLoc.NUMBER, numberProto)
      .update(addrManager.ProtoLoc.STRING, stringProto)

      .update(addrManager.ErrorLoc.ERR, err)
      .update(addrManager.ErrorLoc.EVAL_ERR, evalErr)
      .update(addrManager.ErrorLoc.RANGE_ERR, rangeErr)
      .update(addrManager.ErrorLoc.REF_ERR, refErr)
      .update(addrManager.ErrorLoc.SYNTAX_ERR, syntaxErr)
      .update(addrManager.ErrorLoc.TYPE_ERR, typeErr)
      .update(addrManager.ErrorLoc.URI_ERR, uriErr)

      .update(addrManager.ProtoLoc.ERR, errProto)
      .update(addrManager.ProtoLoc.EVAL_ERR, evalErrProto)
      .update(addrManager.ProtoLoc.RANGE_ERR, rangeErrProto)
      .update(addrManager.ProtoLoc.REF_ERR, refErrProto)
      .update(addrManager.ProtoLoc.SYNTAX_ERR, syntaxErrProto)
      .update(addrManager.ProtoLoc.TYPE_ERR, typeErrProto)
      .update(addrManager.ProtoLoc.URI_ERR, uriErrProto)

      .update(addrManager.ConstructorLoc.OBJ, objConstructor)
      .update(addrManager.ConstructorLoc.ARRAY, arrayConstructor)

    State(initHeap, Context.Empty)
  }

  def testState: State = {

    val st = state
    val globalObj = st.heap.getOrElse(addrManager.PredefLoc.GLOBAL, utils.ObjEmpty)

    val testGlobalObj =
      globalObj.update("__BOT", utils.PropValueBot)
        .update("__TOP", PropValue(utils.ObjectValueBot.copyWith(Value(utils.PValueTop))))
        .update("__UInt", PropValue(utils.ObjectValueBot.copyWith(utils.absNumber.UInt)))
        .update("__Global", PropValue(utils.ObjectValueBot.copyWith(addrManager.PredefLoc.GLOBAL)))
        .update("__BoolTop", PropValue(utils.ObjectValueBot.copyWith(utils.absBool.Top)))
        .update("__NumTop", PropValue(utils.ObjectValueBot.copyWith(utils.absNumber.Top)))
        .update("__StrTop", PropValue(utils.ObjectValueBot.copyWith(utils.absString.Top)))
        .update("__RefErrLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ErrorLoc.REF_ERR)))
        .update("__RangeErrLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ErrorLoc.RANGE_ERR)))
        .update("__TypeErrLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ErrorLoc.TYPE_ERR)))
        .update("__URIErrLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ErrorLoc.URI_ERR)))
        .update("__RefErrProtoLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ProtoLoc.REF_ERR)))
        .update("__RangeErrProtoLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ProtoLoc.RANGE_ERR)))
        .update("__TypeErrProtoLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ProtoLoc.TYPE_ERR)))
        .update("__URIErrProtoLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ProtoLoc.URI_ERR)))
        .update("__ErrProtoLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ProtoLoc.ERR)))
        .update("__ObjConstLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ConstructorLoc.OBJ)))
        .update("__ArrayConstLoc", PropValue(utils.ObjectValueBot.copyWith(addrManager.ConstructorLoc.OBJ))) //TODO check tests/js/semantics/load11.js

    val testHeap = st.heap.update(addrManager.PredefLoc.GLOBAL, testGlobalObj)
    State(testHeap, st.context)
  }
}