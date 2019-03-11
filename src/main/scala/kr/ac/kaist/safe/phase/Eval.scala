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
import kr.ac.kaist.safe.nodes.core.{ ISeq, State }
import kr.ac.kaist.safe.util._
import scala.util.{ Try, Success }

// Eval phase
case object Eval extends PhaseObj[ISeq, EvalConfig, State] {
  val name: String = "eval"
  val help: String = "evaluates JavaScript source files to Core."

  def apply(
    core: ISeq,
    safeConfig: SafeConfig,
    config: EvalConfig
  ): Try[State] = {
    // Evaluate Core.
    val st: State = core.interp

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

  def defaultConfig: EvalConfig = EvalConfig()
  val options: List[PhaseOption[EvalConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during evaluation are muted."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the result of evaluation will be written to the outfile.")
  )
}

// Eval phase config
case class EvalConfig(
  var silent: Boolean = false,
  var outFile: Option[String] = None
) extends Config
