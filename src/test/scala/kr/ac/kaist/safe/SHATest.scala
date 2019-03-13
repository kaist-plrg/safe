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
import org.scalatest._

class SHATest extends AnalyzeTest {
  // registration
  val shaTestDir = testDir + "sha"
  analyzeHelper("SHA", List(shaTestDir))
}
