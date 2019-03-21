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
import kr.ac.kaist.safe.nodes.core._
import scala.util.{ Try, Failure }
import kr.ac.kaist.safe.{ LINE_SEP, SafeConfig }
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.errors.error.NoFileError

// ParseCore phase
case object ParseCore extends PhaseObj[Unit, ParseCoreConfig, Program] {
  val name = "core-parser"
  val help = "ParseCores files." + LINE_SEP +
    "If multiple files are given, they are concatenated in the given order before being parsed."

  def apply(
    unit: Unit,
    safeConfig: SafeConfig,
    config: ParseCoreConfig
  ): Try[Program] = safeConfig.fileNames match {
    case Nil => Failure(NoFileError("core-parser"))
    case _ => Program.filesToCore(safeConfig.fileNames).map {
      case program => {
        // Pretty print to file.
        config.outFile match {
          case Some(out) => {
            val (fw, writer) = Useful.fileNameToWriters(out)
            writer.write(program.toString)
            writer.close; fw.close
            println("Dumped parsed Core code to " + out)
          }
          case None =>
        }

        program
      }
    }
  }

  def defaultConfig: ParseCoreConfig = ParseCoreConfig()
  val options: List[PhaseOption[ParseCoreConfig]] = List(
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the parsed Core code will be written to the outfile.")
  )
}

// ParseCore phase config
case class ParseCoreConfig(
  var outFile: Option[String] = None
) extends Config
