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

// CORE Environments
case class Env(ids: Map[Id, Value], labels: Map[Label, Cont]) {
  // update identifiers
  def update(id: Id, value: Value): Env =
    copy(ids = ids + (id -> value))

  // update labels
  def update(label: Label, cont: Cont): Env =
    copy(labels = labels + (label -> cont))

  // lookup identifiers
  def apply(id: Id): Value =
    ids.getOrElse(id, error(s"free identifier: $id"))

  // lookup labels
  def apply(label: Label): Cont =
    labels.getOrElse(label, error(s"free label: $label"))
}
