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

package kr.ac.kaist.safe.nodes.core

import java.io._
import kr.ac.kaist.safe.LINE_SEP

// CORE Global Model Loader
object GlobalLoader {
  def apply(filename: String): Map[Id, Value] = {
    var map: Map[Id, Value] = Map()
    for (file <- walkTree(new File(filename))) {
      val filename = file.getName
      if (funcFilter(filename)) {
        val id = Id(filename.dropRight(5))
        val func = Parser.fileToFunc(file.toString)
        map += (id -> func)
      }
    }
    map
  }

  private def walkTree(file: File): Iterable[File] = {
    val children = new Iterable[File] {
      def iterator: Iterator[File] = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
    }
    Seq(file) ++: children.flatMap(walkTree(_))
  }

  private def extFilter(ext: String): String => Boolean = _.endsWith(s".$ext")
  private val funcFilter = extFilter("func")
}
