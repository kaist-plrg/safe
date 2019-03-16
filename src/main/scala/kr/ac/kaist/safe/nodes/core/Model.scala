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

// CORE Model
abstract class Model {
  val env: Env
  val heap: Heap
}
object Model {
  def unapply(model: Model): Option[(Env, Heap)] = Some((model.env, model.heap))
}

object ECMAScript5 extends Model {
  // environment
  val env = Env(Map(
    UserId("$ERROR") -> Clo(List(UserId("x")), List(IThrow(EId(UserId("x"))))),
    UserId("$FAIL") -> Clo(List(UserId("x")), List(IThrow(EId(UserId("x"))))),
    UserId("runTestCase") -> Clo(List(UserId("x")), List(IIf(EId(UserId("x")), Nil, List(IThrow(EId(UserId("x"))))))),
    UserId("fnGlobalObject") -> Clo(List(), List(IReturn(EId(UserId("[[Global]]")))))
  ), Map())

  // heap
  val heap = Heap(Map())
}
