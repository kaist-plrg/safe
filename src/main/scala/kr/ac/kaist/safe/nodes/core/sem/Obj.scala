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
case class Obj(ty: Ty, map: Map[Id, Value] = Map()) {
  // update properties
  def update(id: Id, value: Value): Obj = copy(map = map + (id -> value))

  // lookup properties
  def apply(id: Id): Value =
    map.getOrElse(id, error(s"free property: $id"))

  // delete properties
  def delete(id: Id): Obj = copy(map = map - id)

  // check existence
  def contains(id: Id): Value = Bool(map contains id)

  // get type as string
  def typeAsStr: Str = Str(ty)

  def appendTo(
    sb: StringBuilder,
    indent: String = "",
    firstIndent: Boolean = true,
    detail: Boolean = true
  ): StringBuilder = appendMap(sb, map, indent, detail)
}
