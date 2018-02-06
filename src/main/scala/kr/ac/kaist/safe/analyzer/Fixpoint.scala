/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.console.Interactive
import kr.ac.kaist.safe.nodes.cfg.{ CFGEdgeExc, CFGEdgeNormal }

import scala.collection.immutable.HashMap

class Fixpoint(
    semantics: Semantics,
    val consoleOpt: Option[Interactive]
) {
  def worklist: Worklist = semantics.worklist

  def compute(initIters: Int = 0): Int = {
    var iters = initIters
    while (!worklist.isEmpty) {
      iters += 1
      computeOneStep()
    }
    consoleOpt.foreach(_.runFinished)
    iters
  }

  def computeOneStep(): Unit = {
    consoleOpt.foreach(_.runFixpoint)

    // set of control points.
    val cps = worklist.pop

    cps.foreach(cp => {
      val st = semantics.getState(cp)
      val (nextSts, nextExcSts, nextInters) = semantics.C(cp, st)

      nextSts.foreach(propagateNormal(cp, _))
      nextExcSts.foreach(propagateException(cp, _))
      nextInters.foreach(propagateInterProc(cp, _))
    })
  }

  def propagateNormal(cp: ControlPoint, ns: (ControlPoint, AbsState)): Unit = {
    // Propagate normal output state (outS) along normal edges.
    val (succCP, nextSt) = ns
    val oldSt = semantics.getState(succCP)
    if (!(nextSt ⊑ oldSt)) {
      val newSt = oldSt ⊔ nextSt
      semantics.setState(succCP, newSt)
      worklist.add(succCP)
    }
  }

  def propagateException(cp: ControlPoint, ns: (ControlPoint, AbsState)): Unit = {
    // Propagate exception output state (outES) along exception edges.
    // 1) If successor is catch, current exception value is assigned to catch variable and
    //    previous exception values are restored.
    // 2) If successor is finally, current exception value is propagated further along
    //    finally block's "normal" edges.
    val (excSuccCP, nextExcSt) = ns
    val oldExcSt = semantics.getState(excSuccCP)
    if (!(nextExcSt ⊑ oldExcSt)) {
      val newExcSet = oldExcSt ⊔ nextExcSt
      semantics.setState(excSuccCP, newExcSet)
      worklist.add(excSuccCP)
    }
  }

  def propagateInterProc(cp: ControlPoint, ns: (ControlPoint, (EdgeData, AbsState))): Unit = {
    // Propagate along inter-procedural edges
    // This step must be performed after evaluating abstract transfer function
    // because 'call' instruction can add inter-procedural edges.
    val (succCP, (data, nextSt)) = ns

    val oldSt = semantics.getState(succCP)
    val nextSt2 = semantics.E(cp, succCP, data, nextSt)
    if (!(nextSt2 ⊑ oldSt)) {
      val newSt = oldSt ⊔ nextSt2
      semantics.setState(succCP, newSt)
      worklist.add(succCP)
    }
  }
}
