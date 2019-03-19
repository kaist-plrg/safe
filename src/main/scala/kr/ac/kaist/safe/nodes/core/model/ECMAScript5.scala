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

// ECMASCript 5.1
object ECMAScript5 extends Model {
  // internal identifiers
  val GlobalId = InternalId("Global")

  val failClo = Clo("(x) => throw x;")
  val assertClo = Clo("(x) => assert x;")

  // environment
  val env: Env = Env(Map(
    UserId("$ERROR") -> failClo,
    UserId("$FAIL") -> failClo,
    UserId("runTestCase") -> assertClo,
    UserId("fnGlobalObject") -> Clo(List(), List(IReturn(EId(GlobalId))))
  ), Map(), None, None)

  // heap
  val heap: Heap = Heap(Map())
}
