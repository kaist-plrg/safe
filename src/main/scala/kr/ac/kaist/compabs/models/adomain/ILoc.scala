/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.adomain

import scala.collection.mutable
import scala.util.matching.Regex

/**
 * Created by ysko on 2017. 2. 27..
 */
trait AbsDOMLoc {
  trait AbsDOMLocT {
    def ==(that: T): Boolean
    val isRecent: Boolean
    val allocSite: Long
    def toOld: T
  }
  type T <: AbsDOMLocT

  def sys(name: String): T
  def recent(a: Long): T
  def old(a: Long): T
  val nullv: T
  def parseLocName(s: String): Option[T]
}

object ILoc {
  def Make(): AbsDOMLoc = new AbsDOMLoc {
    case class T(allocSite: Long, recent: Boolean) extends AbsDOMLocT {
      def ==(that: T): Boolean = this.allocSite == that.allocSite && this.recent == that.recent
      val isRecent: Boolean = recent
      def toOld: T = T(allocSite, recent = false)

      override def toString: String = {
        val tag = if (recent) "I#" else "I##"
        val name =
          if (allocSite == 0) "null"
          else longToSystem.getOrElse(allocSite, allocSite.toString)
        s"$tag$name"
      }
    }

    var allocToRecentLong: mutable.HashMap[Long, Long] = new mutable.HashMap[Long, Long]()

    var addr: Long = 1
    def recent(a: Long): T = T(a, recent = true)
    def old(a: Long): T = T(a, recent = false)

    var system: Long = -1
    var systemToLong: mutable.HashMap[String, Long] = new mutable.HashMap[String, Long]()
    var longToSystem: mutable.HashMap[Long, String] = new mutable.HashMap[Long, String]()
    def sys(name: String): T = {
      if (name == null) nullv
      else {
        val (g, recent) =
          if (name.startsWith("##"))
            (name.substring(2), false)
          else if (name.startsWith("#"))
            (name.substring(1), true)
          else throw new InternalError("Impossible case")

        systemToLong.get(g) match {
          case Some(l) => T(l, recent = recent)
          case None =>
            val n = system
            system = system - 1
            systemToLong += (g -> n)
            longToSystem += (n -> g)
            T(n, recent)
        }
      }
    }
    def parseLocName(s: String): Option[T] = {
      val pattern = new Regex("""(I#|I##)([0-9a-zA-Z.]+)""", "prefix", "locname")
      def find(addrName: String): Option[Long] = {
        systemToLong.get(addrName)
      }

      try {
        val pattern(prefix, locname) = s
        val r = prefix match {
          case "I#" => true
          case "I##" => false
        }
        val address = find(locname) match {
          case Some(addr) => addr
          case None => locname.toInt
        }
        Some(T(address, r))
      } catch {
        case e: MatchError => None
        case e: NumberFormatException => None
      }
    }

    val nullv: T = T(0, recent = true)
  }
}
