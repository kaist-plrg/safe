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

package kr.ac.kaist.safe.phase

import kr.ac.kaist.safe.nodes.core._
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.{ LINE_SEP, SafeConfig }
import scala.util.{ Try, Success }

// REPLCore phase
case object REPLCore extends PhaseObj[Unit, REPLCoreConfig, Unit] {
  val name = "core-repl"
  val help = "REPL for Core syntax."

  def apply(
    unit: Unit,
    safeConfig: SafeConfig,
    config: REPLCoreConfig
  ): Try[Unit] = {
    REPL.run(ECMAScript5, config.detail)
    Success(())
  }

  def defaultConfig: REPLCoreConfig = REPLCoreConfig()
  val options: List[PhaseOption[REPLCoreConfig]] = List(
    ("detail", BoolOption(c => c.detail = true),
      "Show detailed status of the current state.")
  )
}

// REPLCore phase config
case class REPLCoreConfig(
  var detail: Boolean = false
) extends Config
