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

// Semantics Model
abstract class Model {
  // environment
  val globals: Map[Id, Value]

  // heap
  val heap: Heap

  // get initial state
  def getInitial(pgm: Program): State = {
    val (initialLocals, initialHeap) = heap.allocLocals()
    val initialEnv: Env = Env(
      locals = initialLocals,
      retCont = None,
      excCont = None
    )
    State(
      insts = pgm.insts,
      globals = globals,
      env = initialEnv,
      heap = initialHeap
    )
  }
}
