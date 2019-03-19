/**
 * *****************************************************************************
 * Copyright (c) 2016-2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe

import java.io._
import kr.ac.kaist.safe.phase._
import kr.ac.kaist.safe.nodes.core.State
import org.scalatest._
import scala.util.Random.shuffle

class EvalTest extends SafeTest {
  // registration
  val evalTestDir = testDir + "core"
  shuffle(walkTree(new File(evalTestDir))).foreach(file => {
    val filename = file.getName
    lazy val name = file.toString
    lazy val config = safeConfig.copy(fileNames = List(name))
    if (jsFilter(filename)) test(s"[Eval] $filename") {
      assert(CmdEval(List("-silent", name)).isSuccess)
    }
    else if (errFilter(filename)) test(s"[Eval] $filename") {
      assert(CmdEval(List("-silent", name)).isFailure)
    }
  })
}
