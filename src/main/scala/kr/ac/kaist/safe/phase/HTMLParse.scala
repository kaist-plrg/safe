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

package kr.ac.kaist.safe.phase

import scala.util.{ Failure, Success, Try }
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.html.HTMLModel
import kr.ac.kaist.safe.nodes.ast.Program
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.errors.error.{ NoFileError, NotHTMLFileError, NotHTMLsError }
import kr.ac.kaist.safe.parser.Parser

// Parse phase
case object HTMLParse extends PhaseObj[Unit, HTMLParseConfig, (HTMLModel.T, Program)] {
  val name = "HTMLParser"
  val help = "Parses HTML files."

  def apply(
    unit: Unit,
    safeConfig: SafeConfig,
    config: HTMLParseConfig
  ): Try[(HTMLModel.T, Program)] = {
    safeConfig.fileNames match {
      case Nil => Failure(NoFileError("parse"))
      case file :: Nil if HTMLModel.isSupported(file) =>
        val html = HTMLModel.parseHTMLFromFile(file)
        val code = HTMLModel.getProgram(html)

        Parser.codeToAST(code).get match {
          case (program, excLog) =>
            // Report errors.
            if (excLog.hasError) {
              println(program.relFileName + ":")
              println(excLog)
            }

            // Pretty print to file.
            config.outFile match {
              case Some(out) => {
                val (fw, writer) = Useful.fileNameToWriters(out)
                writer.write(program.toString(0))
                writer.close; fw.close
                println("Dumped parsed JavaScript code to " + out)
              }
              case None =>
            }

            Success(html, program)
        }

      case file :: _ =>
        if (!HTMLModel.isSupported(file)) Failure(NotHTMLFileError(file))
        else Failure(NotHTMLsError)
    }
  }

  def defaultConfig: HTMLParseConfig = HTMLParseConfig()
  val options: List[PhaseOption[HTMLParseConfig]] = List(
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the parsed JavaScript code will be written to the outfile.")
  )
}

// HTML Parse phase config
case class HTMLParseConfig(
  var outFile: Option[String] = None
) extends Config
