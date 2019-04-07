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

import kr.ac.kaist.safe._

// ECMASCript 5.1
object ECMAScript5 extends Model {
  val ExecutionContext = NamedAddr("ExecutionContext")
  val GlobalEnvironment = NamedAddr("GlobalEnvironment")
  val GlobalObjectEnvironmentRecord = NamedAddr("GlobalObjectEnvironmentRecord")
  val GlobalObject = NamedAddr("GlobalObject")

  // environment
  val globals: Map[Id, Value] = GlobalLoader(RESRC_DIR + SEP + "coreModels" + SEP + "ECMAScript5.1") + (Id("ExecutionContext") -> ExecutionContext)

  // create new object
  def newObj(typeName: String)(seq: (String, Value)*): Obj = Obj(
    Ty(typeName),
    (seq.map { case (k, v) => Id(k) -> v }).toMap,
    Map()
  )

  // heap
  val heap: Heap = Heap(Map(
    ExecutionContext -> newObj("ExecutionContext")(),
    GlobalEnvironment -> newObj("LexicalEnvironment")(
      "EnvironmentRecord" -> GlobalObjectEnvironmentRecord,
      "OuterEnvironmentReference" -> Null
    ),
    GlobalObjectEnvironmentRecord -> newObj("ObjectEnvironmentRecord")(
      "BindingObject" -> GlobalObject
    ),
    GlobalObject -> newObj("Object")()
  ), size = 0)

  // XXX test262 modeling
  // val failFunc = parseValue("(x) => throw x;")
  // val assertFunc = parseValue("(x) => assert x;")
  // val globalFunc = parseValue("() => return @Global;")
  // UserId("$ERROR") -> failFunc,
  // UserId("$FAIL") -> failFunc,
  // UserId("runTestCase") -> assertFunc,
  // UserId("fnGlobalObject") -> globalFunc
}
