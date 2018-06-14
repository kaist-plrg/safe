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
import org.w3c.dom.{ Document, Node }

trait DOMModel {
  val path: String

  type AAbsState
  type AAbsValue
  type AAbsBool
  type AAbsStr
  type AAbsLoc
  type ALoc
  type AAbsIValue
  type AAbsDObject
  type AAbsObj
  type AAbsDataProp
  type AAbsNum
  type AException
  type FuncParser = (String, Option[(String, Int, Int)]) => AAbsObj

  type T

  val Parser: HTMLParser
  def parseHTMLFromFile(name: String): T
  def toDOM(t: T): Document
  def genInitialDOMTree(nodet: Document, getSpan: Node => Option[(String, Int, Int)], s: AAbsState, parseFunction: FuncParser): AAbsState
  def getProgram(t: T): List[CodeFragment]
  def isSupported: String => Boolean
  def loadModel: Host
  def init(t: T, s: AAbsState): AAbsState
  def genSemantics(): String => (AAbsValue, AAbsState) => (AAbsState, AAbsState, AAbsValue)
}