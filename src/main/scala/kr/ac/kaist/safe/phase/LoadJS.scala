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
import kr.ac.kaist.safe.compiler.{ JSLoader, DeclCollector }
import kr.ac.kaist.safe.nodes.ast
import kr.ac.kaist.safe.nodes.core
import kr.ac.kaist.safe.util._

// LoadJS phase
case object LoadJS extends PhaseObj[ast.Program, LoadJSConfig, core.State] {
  val name: String = "load-js"
  val help: String = "Load JavaScript program into Core State"

  def apply(
    pgm: ast.Program,
    safeConfig: SafeConfig,
    config: LoadJSConfig
  ): Try[core.State] = {
    // Collect declarations
    val program = (new DeclCollector).walk(pgm)

    // AST -> State.
    val loader = new JSLoader(program, core.ECMAScript5)
    val st = loader.result

    // Pretty print to file.
    config.outFile match {
      case Some(out) => {
        val ((fw, writer)) = Useful.fileNameToWriters(out)
        writer.write(core.beautify(st))
        writer.close; fw.close
        println("Dumped core to " + out)
      }
      case None =>
    }
    Success(st)
  }

  def defaultConfig: LoadJSConfig = LoadJSConfig()
  val options: List[PhaseOption[LoadJSConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during compilation are muted."),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the resulting Core will be written to the outfile.")
  )
}

// LoadJS phase config
case class LoadJSConfig(
  var silent: Boolean = false,
  var outFile: Option[String] = None
) extends Config
