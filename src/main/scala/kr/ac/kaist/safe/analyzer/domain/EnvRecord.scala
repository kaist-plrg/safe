/**
 * *****************************************************************************
 * Copyright (c) 2016, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer.domain

import kr.ac.kaist.safe.LINE_SEP
import kr.ac.kaist.safe.util.Loc
import scala.collection.immutable.HashMap

// 10.2.1 Environment Records
abstract class EnvRecord {
  // HasBinding(N)
  def HasBinding(name: String)(boolU: AbsBoolUtil): AbsBool

  // CreateMutableBinding(N, D)
  def CreateMutableBinding(name: String, del: Boolean): Unit

  // SetMutableBinding(N, V, S)
  def SetMutableBinding(name: String, v: Value, strict: Boolean): Set[Exception]

  // GetBindingValue(N, S)
  def GetBindingValue(name: String, strict: Boolean): Set[Exception]

  // DeleteBinding(N)
  def DeleteBinding(name: String): AbsBool

  // ImplicitThisValue()
  def ImplicitThisValue: Value
}