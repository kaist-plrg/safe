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
import kr.ac.kaist.safe.analyzer.domain.{ DefaultBool, DefaultLoc, DefaultNull, DefaultNumber, DefaultUndef, RecencyAAddr, StringSet, register }
import kr.ac.kaist.safe.html.ModelConverter
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.errors.error.NoFileError

// Parse phase
case object ModelConvert extends PhaseObj[Unit, ConvertConfig, Unit] {
  val name = "ModelConvert"
  val help = "Convert host environment models."

  def apply(
    unit: Unit,
    safeConfig: SafeConfig,
    config: ConvertConfig
  ): Try[Unit] = {
    register(
      DefaultUndef,
      DefaultNull,
      DefaultBool,
      DefaultNumber,
      StringSet(0),
      DefaultLoc,
      RecencyAAddr
    )

    safeConfig.fileNames match {
      case Nil => Failure(NoFileError("parse"))
      case files => Success(ModelConverter.convertModels(files, config.outFile.getOrElse("out")))
    }
  }

  def defaultConfig: ConvertConfig = ConvertConfig()
  val options: List[PhaseOption[ConvertConfig]] = List(
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the converted host environment structure will be written into this file.")
  )
}

// HTML Parse phase config
case class ConvertConfig(
  var outFile: Option[String] = None
) extends Config
