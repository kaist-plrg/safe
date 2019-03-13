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

object Model {
  val initial = State(Env(Map(
    // for test262
    "$ERROR" -> Clo(List("x"), IThrow("x"), Env()),
    "$FAIL" -> Clo(List("x"), IThrow("x"), Env()),
    "runTestCase" -> Clo(List("x"), IIf("x", ISeq(Nil), IThrow("x")), Env()),
    "fnglobalObject" -> Clo(List(), INotYetImpl, Env())
  )), Heap())
}
