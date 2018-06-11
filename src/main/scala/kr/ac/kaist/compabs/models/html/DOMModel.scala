/**
 * *****************************************************************************
 * Copyright (c) 2015-2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.html

import kr.ac.kaist.compabs.models.html.parser.HTMLParser
import kr.ac.kaist.compabs.models.shape.Host

trait DOMModel {
  val path: String

  type AAbsState
  type AAbsValue
  type AAbsBool
  type AAbsStr
  type AAbsNum
  type AException

  type T

  val Parser: HTMLParser
  def parseHTMLFromFile(name: String): T
  def getProgram(t: T): List[CodeFragment]
  def isSupported: String => Boolean
  def loadModel: Host

  def genSemantics(): String => (AAbsValue, AAbsState) => (AAbsState, AAbsState, AAbsValue)
}