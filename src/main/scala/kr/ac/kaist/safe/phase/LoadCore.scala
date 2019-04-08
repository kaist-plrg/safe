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

import scala.util.{ Try, Success }
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.nodes.core._
import kr.ac.kaist.safe.util._

// LoadCore phase
case object LoadCore extends PhaseObj[Program, LoadCoreConfig, State] {
  val name: String = "load-core"
  val help: String = "Load Core program into Core State"

  def apply(
    pgm: Program,
    safeConfig: SafeConfig,
    config: LoadCoreConfig
  ): Try[State] = {
    // Evaluate Core program
    val st: State = ECMAScript5.getInitial(pgm)

    // Pretty print to file.
    config.outFile match {
      case Some(out) => {
        val ((fw, writer)) = Useful.fileNameToWriters(out)
        writer.write(beautify(st))
        writer.close; fw.close
        println("Dumped core to " + out)
      }
      case None =>
    }
    Success(st)
  }

  def defaultConfig: LoadCoreConfig = LoadCoreConfig()
  val options: List[PhaseOption[LoadCoreConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during compilation are muted."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the resulting Core will be written to the outfile.")
  )
}

// LoadCore phase config
case class LoadCoreConfig(
  var silent: Boolean = false,
  var outFile: Option[String] = None
) extends Config
