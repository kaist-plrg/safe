/**
 * *****************************************************************************
 * Copyright (c) 2019, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by thcored parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.phase

import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.nodes.core._
import kr.ac.kaist.safe.util._
import scala.util.{ Try, Success }

// EvalCore phase
case object EvalCore extends PhaseObj[State, EvalCoreConfig, State] {
  val name: String = "core-interpreter"
  val help: String = "evaluates JavaScript source files to Core."

  def apply(
    initialSt: State,
    safeConfig: SafeConfig,
    config: EvalCoreConfig
  ): Try[State] = {
    // Evaluate Core program
    val st: State = Interp.fixpoint(initialSt)

    // Pretty print to file.
    config.outFile match {
      case Some(out) => {
        val ((fw, writer)) = Useful.fileNameToWriters(out)
        writer.write(st.toString)
        writer.close; fw.close
        println("Dumped the result to " + out)
      }
      case None =>
    }
    Success(st)
  }

  def defaultConfig: EvalCoreConfig = EvalCoreConfig()
  val options: List[PhaseOption[EvalCoreConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during evaluation are muted."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the result of evaluation will be written to the outfile.")
  )
}

// EvalCore phase config
case class EvalCoreConfig(
  var silent: Boolean = false,
  var outFile: Option[String] = None
) extends Config
