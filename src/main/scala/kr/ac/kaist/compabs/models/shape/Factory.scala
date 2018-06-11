/**
 * *****************************************************************************
 * Copyright (c) 2015-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.shape

import kr.ac.kaist.compabs.models.cdomain.{ CLoc, CValue }

object Factory {
  def emptyHObjList: List[HObject] = Nil
  def emptyHPropList: List[HProp] = Nil
  def singleList[T](o: T): List[T] = List(o)
  def genPropValue(e: java.util.Map[String, Object]): HUPropValue = {
    val w: Option[Boolean] =
      if (e.containsKey("w")) Some(e.get("w").asInstanceOf[Boolean])
      else None
    val c: Option[Boolean] =
      if (e.containsKey("c")) Some(e.get("c").asInstanceOf[Boolean])
      else None
    val enum: Option[Boolean] =
      if (e.containsKey("e")) Some(e.get("e").asInstanceOf[Boolean])
      else None
    val v: Option[CValue] =
      if (e.containsKey("v")) Some(e.get("v").asInstanceOf[CValue])
      else None
    val s: Option[CLoc] =
      if (e.containsKey("s")) Some(e.get("s").asInstanceOf[CLoc])
      else None
    val g: Option[CLoc] =
      if (e.containsKey("g")) Some(e.get("g").asInstanceOf[CLoc])
      else None
    val gs: Option[CValue] =
      if (e.containsKey("gs")) Some(e.get("gs").asInstanceOf[CValue])
      else None

    HUPropValue(w, enum, c, v, s, g, gs)
  }

  def lineTerminating(c: Char): Boolean =
    List('\u000a', '\u2028', '\u2029', '\u000d').contains(c)

  def unescapeJava(s: String) =
    if (-1 == s.indexOf('\\')) s
    else {
      val length = s.length
      val buf = new StringBuilder(length)
      var i = 0
      while (i < length) {
        var c = s.charAt(i)
        if ('\\' != c) {
          buf.append(c)
          i += 1
        } else {
          i += 1
          if (i >= length) {
            throw new IllegalArgumentException("incomplete escape sequence")
          }
          c = s.charAt(i)
          c match {
            case '"' => buf.append('"')
            case '\'' => buf.append('\'')
            case '\\' => buf.append('\\')
            case 'b' => buf.append('\b')
            case 'f' => buf.append('\f')
            case 'n' => buf.append('\n')
            case 'r' => buf.append('\r')
            case 't' => buf.append('\t')
            case 'v' => buf.append('\u000b')
            case 'x' =>
              i += 2
              if (i >= length) {
                throw new IllegalArgumentException("incomplete universal character" +
                  " name " + s.substring(i - 1))
              }
              val n = Integer.parseInt(s.substring(i - 1, i + 1), 16)
              buf.append(n.asInstanceOf[Char])
            case 'u' =>
              i += 4
              if (i >= length) {
                throw new IllegalArgumentException("incomplete universal character" +
                  " name " + s.substring(i - 3))
              }
              val n = Integer.parseInt(s.substring(i - 3, i + 1), 16)
              buf.append(n.asInstanceOf[Char])
            case cc if lineTerminating(cc) =>
            case _ => buf.append(c)
          }
          i += 1
        }
      }
      buf.toString
    }
}
