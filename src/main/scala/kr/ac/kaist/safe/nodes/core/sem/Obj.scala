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

import kr.ac.kaist.safe.LINE_SEP

// CORE Objects
case class Obj(map: Map[Id, Value] = Map()) {
  // update properties
  def update(id: Id, value: Value): Obj = Obj(map + (id -> value))

  // lookup properties
  def apply(id: Id): Value =
    map.getOrElse(id, error(s"free property: $id"))

  // delete properties
  def delete(id: Id): Obj = Obj(map - id)

  // check existence
  def contains(id: Id): Value = Bool(map contains id)
}
object Obj {
  def apply(seq: (Id, Value)*): Obj = Obj(Map(seq: _*))
}
