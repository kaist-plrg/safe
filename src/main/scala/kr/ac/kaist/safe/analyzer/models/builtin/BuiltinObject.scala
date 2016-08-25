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

package kr.ac.kaist.safe.analyzer.models.builtin

import kr.ac.kaist.safe.analyzer.{ Semantics, Helper }
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.models._
import kr.ac.kaist.safe.util.{ SystemAddr, Loc, Recent }

// 15.2 Object Objects
object BuiltinObject extends FuncModel(
  name = "Object",

  // 15.2.1 The Object Constructor Called as a Function: Object([value])
  code = BasicCode(argLen = 1, (
    args: Value, st: State, sem: Semantics, utils: Utils
  ) => {
    val h = st.heap
    val argV = sem.CFGLoadHelper(args, Set(utils.absString.alpha("0")), h)
    val addr = SystemAddr("Object<instance>")

    // 1. If value is null, undefined or not supplied, create and return
    //    a new Object object exactly as if the standard built-in Object
    //    constructor had been called with the same arguments.
    val pv = argV.pvalue
    val (v1, st1) = if (pv.undefval.gamma != ConSimpleBot ||
      pv.nullval.gamma != ConSimpleBot ||
      argV.isBottom) {
      val state = st.oldify(addr)(utils)
      val loc = Loc(addr, Recent)
      val obj = Obj.newObject(utils)
      val heap = state.heap.update(loc, obj)
      val ctx = state.context
      (Value(loc)(utils), State(heap, ctx))
    } else {
      (Value.Bot(utils), State.Bot)
    }

    // 2. Return ToObject(value)
    val (v2, st2, _) = Helper(utils).toObject(st, argV, addr)

    (st1 + st2, State.Bot, v1 + v2)
  }),

  // 15.2.2 The Object Constructor: new Object([value])
  construct = Some(BasicCode(argLen = 1, (
    args: Value, st: State, sem: Semantics, utils: Utils
  ) => {
    val h = st.heap
    val argV = sem.CFGLoadHelper(args, Set(utils.absString.alpha("0")), h)
    val addr = SystemAddr("Object<instance>")

    // 1. If value is supplied, then
    //    a. If Type(value) is Object, then
    //       i. If the value is a native ECMAScript object, do not create a new object
    //          but simply return value.
    //       ii. If the value is a host object, then actions are taken and a result is
    //           returned in an implementation-dependent manner that may depend on
    //           the host object.
    //           (We do not consider an implementation-dependent actions for a host object)
    //    b. If Type(value) is String, return ToObject(value).
    //    c. If Type(value) is Boolean, return ToObject(value).
    //    d. If Type(value) is Number, return ToObject(value).
    val (v2, st2, _) = Helper(utils).toObject(st, argV, addr)

    // 2. Assert: The argument value was not supplied or its type was Null or Undefined.
    // 3. Let obj be a newly created native ECMAScript object.
    val pv = argV.pvalue
    val (v1, st1) = if (pv.undefval.gamma != ConSimpleBot ||
      pv.nullval.gamma != ConSimpleBot ||
      argV.isBottom) {
      val state = st.oldify(addr)(utils)
      val loc = Loc(addr, Recent)
      val obj = Obj.newObject(utils)
      val heap = state.heap.update(loc, obj)
      val ctx = state.context
      (Value(loc)(utils), State(heap, ctx))
    } else {
      (Value.Bot(utils), State.Bot)
    }

    (st1 + st2, State.Bot, v1 + v2)
  })),

  // 15.2.3.1 Object.prototype
  protoModel = Some((BuiltinObjectProto, F, F, F)),

  props = List(
    // 15.2.3.2 Object.getPrototypeOf(O)
    ("getPrototypeOf", FuncModel(
      name = "Object.getPrototypeOf",
      code = BasicCode(argLen = 1, (
        args: Value, st: State, sem: Semantics, utils: Utils
      ) => {
        val h = st.heap
        val argV = sem.CFGLoadHelper(args, Set(utils.absString.alpha("0")), h)
        val tmpAddr = SystemAddr("<temp>")

        val (retV, retSt, excSet) = Helper(utils).toObject(st, argV, tmpAddr)

        // 1. If Type(O) is not Object throw a TypeError exception.
        val excSt = st.raiseException(excSet)(utils)

        // 2. Return the value of [[Prototype]] internal property of O.
        val protoV = retV.locset.foldLeft(Value.Bot(utils))((v, loc) => {
          v + retSt.heap(loc).getOrElse(Obj.Bot(utils)).get("@proto")(utils).objval.value
        })

        (st, excSt, protoV)
      })
    ), T, F, T),

    // 15.2.3.3 getOwnPropertyDescriptor(O, P)
    ("getOwnPropertyDescriptor", FuncModel(
      name = "Object.getOwnPropertyDescriptor",
      code = BasicCode(argLen = 2, (
        args: Value, st: State, sem: Semantics, utils: Utils
      ) => {
        val h = st.heap
        val objV = sem.CFGLoadHelper(args, Set(utils.absString.alpha("0")), h)
        val strV = sem.CFGLoadHelper(args, Set(utils.absString.alpha("1")), h)
        val tmpAddr = SystemAddr("<temp>")
        val descAddr = SystemAddr("Object.getOwnPropertyDescriptor<descriptor>")
        val AT = utils.absBool.alpha(true)
        val AF = utils.absBool.alpha(false)

        val (locV, retSt, excSet) = Helper(utils).toObject(st, objV, tmpAddr)

        // 1. If Type(O) is not Object throw a TypeError exception.
        val excSt = st.raiseException(excSet)(utils)

        // 2. Let name be ToString(P).
        val name = strV
          .toPrimitiveBetter(h)(utils)
          .toAbsString(utils.absString)

        // 3. Let desc be the result of calling the [[GetOwnProperty]]
        //    internal method of O with argument name.
        // 4. Return the result of calling FromPropertyDescriptor(desc)
        val obj = locV.locset.foldLeft(Obj.Bot(utils))((obj, loc) => {
          obj + retSt.heap.getOrElse(loc, Obj.Bot(utils))
        })
        val isDomIn = (obj domIn name)(utils.absBool)
        val v1 =
          if (AF <= isDomIn) Value(PValue(utils.absUndef.Top)(utils))
          else Value.Bot(utils)
        val (state, v2) =
          if (AT <= isDomIn) {
            val objval = obj(name).getOrElse(PropValue.Bot(utils)).objval
            val valueV = objval.value
            val writableV = Value(PValue(objval.writable)(utils))
            val enumerableV = Value(PValue(objval.enumerable)(utils))
            val configurableV = Value(PValue(objval.configurable)(utils))
            val descObj = Obj.newObject(utils)
              .update("value", PropValue(ObjectValue(valueV, AT, AF, AT)))
              .update("writable", PropValue(ObjectValue(writableV, AT, AF, AT)))
              .update("enumerable", PropValue(ObjectValue(enumerableV, AT, AF, AT)))
              .update("configurable", PropValue(ObjectValue(configurableV, AT, AF, AT)))
            val state = st.oldify(descAddr)(utils)
            val descLoc = Loc(descAddr, Recent)
            val retHeap = state.heap.update(descLoc, descObj)
            val ctx = state.context
            (State(retHeap, ctx), Value(descLoc)(utils))
          } else (st, Value.Bot(utils))

        (state, excSt, v1 + v2)
      })
    ), T, F, T),

    // TODO getOwnPropertyNames
    ("getOwnPropertyNames", FuncModel(
      name = "Object.getOwnPropertyNames",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO create
    ("create", FuncModel(
      name = "Object.create",
      code = EmptyCode(argLen = 2)
    ), T, F, T),

    // TODO defineProperty
    ("defineProperty", FuncModel(
      name = "Object.defineProperty",
      code = EmptyCode(argLen = 3)
    ), T, F, T),

    // TODO defineProperties
    ("defineProperties", FuncModel(
      name = "Object.defineProperties",
      code = EmptyCode(argLen = 2)
    ), T, F, T),

    // TODO seal
    ("seal", FuncModel(
      name = "Object.seal",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO freeze
    ("freeze", FuncModel(
      name = "Object.freeze",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO preventExtensions
    ("preventExtensions", FuncModel(
      name = "Object.preventExtensions",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO isSealed
    ("isSealed", FuncModel(
      name = "Object.isSealed",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO isFrozen
    ("isFrozen", FuncModel(
      name = "Object.isFrozen",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO isExtensible
    ("isExtensible", FuncModel(
      name = "Object.isExtensible",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO keys
    ("keys", FuncModel(
      name = "Object.keys",
      code = EmptyCode(argLen = 1)
    ), T, F, T)
  )
)

object BuiltinObjectProto extends ObjModel(
  name = "Object.prototype",
  props = List(
    ("@proto", PrimModel(null), F, F, F),

    // TODO toString
    ("toString", FuncModel(
      name = "Object.prototype.toString",
      code = EmptyCode()
    ), T, F, T),

    // TODO toLocaleString
    ("toLocaleString", FuncModel(
      name = "Object.prototype.toLocaleString",
      code = EmptyCode()
    ), T, F, T),

    // TODO valueOf
    ("valueOf", FuncModel(
      name = "Object.prototype.valueOf",
      code = EmptyCode()
    ), T, F, T),

    // TODO hasOwnProperty
    ("hasOwnProperty", FuncModel(
      name = "Object.prototype.hasOwnProperty",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO isPrototypeOf
    ("isPrototypeOf", FuncModel(
      name = "Object.prototype.isPrototypeOf",
      code = EmptyCode(argLen = 1)
    ), T, F, T),

    // TODO propertyIsEnumerable
    ("propertyIsEnumerable", FuncModel(
      name = "Object.prototype.propertyIsEnumerable",
      code = EmptyCode(argLen = 1)
    ), T, F, T)
  )
)
