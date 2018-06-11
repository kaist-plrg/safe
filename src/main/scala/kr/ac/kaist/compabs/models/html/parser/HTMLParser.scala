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
package kr.ac.kaist.compabs.models.html.parser

import java.io.File
import java.net.URL

import kr.ac.kaist.compabs.models.html.CodeFragment
import org.apache.commons.io.FileUtils
import org.w3c.dom.{ Document, Node }

import scala.collection.immutable.HashMap

trait HTMLParser {
  type T

  import kr.ac.kaist.compabs.models.html.EventTypeEnum._

  val supported: Set[String] = Set(".html", ".xhtml", ".htm", ".xhtm")
  def isSupported(file: String): Boolean = supported.exists(p => file.endsWith(p))

  // the following handlers must be supported by all HTML elements
  val events = HashMap[String, EventType](
    "abort" -> Other, "cancel" -> Other,
    "canplay" -> Other, "canplaythrough" -> Other,
    "change" -> Other, "cuechange" -> Other,
    "durationchange" -> Other, "emptied" -> Other,
    "ended" -> Other, "input" -> Other,
    "invalid" -> Other, "loadeddata" -> Other,
    "loadedmetadata" -> Other, "loadstart" -> Other,
    "pause" -> Other, "play" -> Other, "playing" -> Other,
    "progress" -> Other, "ratechange" -> Other, "reset" -> Other,
    "seeked" -> Other, "seeking" -> Other, "select" -> Other,
    "show" -> Other, "stalled" -> Other, "submit" -> Other,
    "suspend" -> Other, "timeupdate" -> Other, "toggle" -> Other,
    "volumechange" -> Other, "waiting" -> Other,

    "keypress" -> Keyboard, "keydown" -> Keyboard, "nkeyup" -> Keyboard, "paste" -> Keyboard,
    "click" -> Mouse, "dblclick" -> Mouse,
    "mouseenter" -> Mouse, "mouseleave" -> Mouse,
    "mousedown" -> Mouse, "mouseup" -> Mouse,
    "mouseover" -> Mouse, "mousemove" -> Mouse, "mouseout" -> Mouse,
    "mousewheel" -> Mouse,
    "selectstart" -> Other
  )

  // all HTML elements other than body and frameset elements.
  val events_set2 = HashMap[String, EventType](
    "load" -> Load,
    "focus" -> Focus, "blur" -> Focus,
    "error" -> Other, "resize" -> Other, "scroll" -> Other
  )

  // must be supported by window objects
  val events_set3 = HashMap[String, EventType](
    "afterprint" -> Other,
    "beforeprint" -> Other,
    "beforeunload" -> Other,
    "hashchange" -> Other,
    "message" -> Message,
    "offline" -> Other,
    "online" -> Other,
    "pagehide" -> Other,
    "pageshow" -> Other,
    "popstate" -> Other,
    "storage" -> Other,
    "unload" -> Unload
  )

  // must be supported by document.
  // The following handlers must be supported on document object.
  val events_set4 = HashMap[String, EventType](
    "readystatechange" -> ReadyStateChange,
    "DOMContentLoaded" -> DOMLoaded
  )

  val events_timer = HashMap[String, EventType](
    "interval" -> Timer,
    "timeout" -> Timer
  )

  val events_sets = events ++ events_set2 ++ events_set3 ++ events_set4 ++ events_timer

  private val tagToClassMap: HashMap[String, String] = HashMap(
    "A" -> "HTMLAnchorElement",
    "APPLET" -> "HTMLAppletElement",
    "AREA" -> "HTMLAreaElement",
    "BASE" -> "HTMLBaseElement",
    "BASEFONT" -> "HTMLBaseFontElement",
    "BLOCKQUOTE" -> "HTMLQuoteElement",
    "BODY" -> "HTMLBodyElement",
    "BR" -> "HTMLBRElement",
    "BUTTON" -> "HTMLButtonElement",
    "DEL" -> "HTMLModElement",
    "DIR" -> "HTMLDirectoryElement",
    "DIV" -> "HTMLDivElement",
    "DL" -> "HTMLDListElement",
    "FIELDSET" -> "HTMLFieldSetElement",
    "FONT" -> "HTMLFontElement",
    "FORM" -> "HTMLFormElement",
    "FRAME" -> "HTMLFrameElement",
    "FRAMESET" -> "HTMLFrameSetElement",
    "HEAD" -> "HTMLHeadElement",
    "H1" -> "HTMLHeadingElement",
    "H2" -> "HTMLHeadingElement",
    "H3" -> "HTMLHeadingElement",
    "H4" -> "HTMLHeadingElement",
    "H5" -> "HTMLHeadingElement",
    "H6" -> "HTMLHeadingElement",
    "HR" -> "HTMLHRElement",
    "HTML" -> "HTMLHtmlElement",
    "IFRAME" -> "HTMLIFrameElement",
    "IMG" -> "HTMLImageElement",
    "INPUT" -> "HTMLInputElement",
    "INS" -> "HTMLModElement",
    //    "ISINDEX" -> "HTMLIsIndexElement", // Isn't supported by Chrome
    "LABEL" -> "HTMLLabelElement",
    "LEGEND" -> "HTMLLegendElement",
    "LI" -> "HTMLLIElement",
    "LINK" -> "HTMLLinkElement",
    "MAP" -> "HTMLMapElement",
    "MENU" -> "HTMLMenuElement",
    "META" -> "HTMLMetaElement",
    "OBJECT" -> "HTMLObjectElement",
    "OL" -> "HTMLOListElement",
    "OPTGROUP" -> "HTMLOptGroupElement",
    "OPTION" -> "HTMLOptionElement",
    "P" -> "HTMLParagraphElement",
    "PARAM" -> "HTMLParamElement",
    "PRE" -> "HTMLPreElement",
    "Q" -> "HTMLQuoteElement",
    "SCRIPT" -> "HTMLScriptElement",
    "SELECT" -> "HTMLSelectElement",
    "STYLE" -> "HTMLStyleElement",
    "TABLE" -> "HTMLTableElement",
    "CAPTION" -> "HTMLTableCaptionElement",
    "TD" -> "HTMLTableCellElement",
    "TH" -> "HTMLTableCellElement",
    "COL" -> "HTMLTableColElement",
    "COLGROUP" -> "HTMLTableColElement",
    "TR" -> "HTMLTableRowElement",
    "TBODY" -> "HTMLTableSectionElement",
    "THEAD" -> "HTMLTableSectionElement",
    "TFOOT" -> "HTMLTableSectionElement",
    "TEXTAREA" -> "HTMLTextAreaElement",
    "TITLE" -> "HTMLTitleElement",
    "UL" -> "HTMLUListElement"
  )

  def tagToClassname(tag: String): String = tagToClassMap.getOrElse(tag.toUpperCase, "HTMLUnknownElement")

  def parseHTMLFromString(html: String, file: String = ""): T
  def parseFragment(markup: String): Node
  def parseHTMLFromFile(file: String): T
  def extractScript(t: T): List[CodeFragment]
  def toDOM(t: T): Document
  def getOptionalSpan(t: T)(n: Node): Option[(String, Int, Int)]
  def createTextNode(s: String): Node
  def createComment(s: String): Node
  def getRootPath(t: T): String

  protected def readContent(t: T)(src: String): CodeFragment = {
    val srcName = stripFilename(src)
    if (src.length == 0) {
      throw new InternalError(s"* Warning: <script> tag doesn't have either contents or proper src filename '$src'.")
    }

    val srcSource = new File(srcName)
    val f_path =
      if (srcSource.isAbsolute) {
        srcName
      } else {
        val path = getRootPath(t)
        if (path == null) srcName
        else path + "/" + srcName
      }

    val objFile = new File(f_path)
    val file: File =
      if (objFile.exists) objFile
      else download(srcName)

    if (file.exists) {
      val source = scala.io.Source.fromFile(file)
      val body = source.mkString.replace("\r", "")
      source.close()
      CodeFragment(f_path, 1, 1, body)
    } else {
      throw new InternalError(s"* Warning: Failed to download a file '$srcName'.")
    }
  }

  private def download(srcName: String): File = {
    val external = new File("downloads/" + srcName.substring(srcName.lastIndexOf('/') + 1))
    if (srcName.endsWith(".js") || srcName.containsSlice(".js")) {
      try {
        val domain = ""
        val url = srcName match {
          case srcname if srcname.startsWith("//") => "http:" + srcname
          case srcname if srcname.startsWith("/") => domain + srcname
          case srcname if srcname.startsWith("file://") => srcname
          case srcname if srcname.startsWith("http") => srcname
          case _ => if (domain.startsWith("http")) domain + "/" + srcName else domain.substring(0, domain.lastIndexOf('/') + 1) + srcName
        }
        FileUtils.copyURLToFile(new URL(url), external)
      } catch {
        case e: Exception =>
          System.err.println("* Warning: Failed to download a file '" + srcName + "'")
      }
    }
    external
  }

  private def stripFilename(src: String): String = {
    val filenameRegexp = ".*/[^/\\:*?\"<>|]+".r // \ : * ? " < > | // file:///c:/~/~/main.js?121424324
    val filename = filenameRegexp.findFirstIn(src).getOrElse(src).trim

    if (filename.toLowerCase.startsWith("file:")) filename.drop(5)
    else filename
  }
}
