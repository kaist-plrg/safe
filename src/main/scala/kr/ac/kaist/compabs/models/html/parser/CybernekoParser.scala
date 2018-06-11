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

import java.io.{ File, FileReader, Reader, StringReader }

import kr.ac.kaist.compabs.models.html.CodeFragment
import org.apache.html.dom.HTMLDocumentImpl
import org.apache.xerces.xni._
import org.apache.xerces.xni.parser.XMLDocumentFilter
import org.cyberneko.html.HTMLScanner.LocationItem
import org.cyberneko.html.filters.DefaultFilter
import org.cyberneko.html.parsers.{ DOMFragmentParser, DOMParser }
import org.w3c.dom.html.HTMLDocument
import org.w3c.dom.{ Document, DocumentFragment, Element, Node }
import org.xml.sax.InputSource

import scala.collection.immutable.HashMap

object CybernekoParser extends HTMLParser {
  case class T(rootPath: String, name: String, root: Document, spans: HashMap[Node, (String, Int, Int)])

  val AUGMENTATIONS = "http://cyberneko.org/html/features/augmentations"

  val types = List("application/ecmascript", "application/javascript", "application/x-ecmascript",
    "application/x-javascript", "text/ecmascript", "text/javascript", "text/javascript1.0",
    "text/javascript1.1", "text/javascript1.2", "text/javascript1.3", "text/javascript1.4",
    "text/javascript1.5", "text/jscript", "text/livescript", "text/x-ecmascript", "text/x-javascript")

  override def parseHTMLFromFile(file: String): T = {
    // Analyzer Configurations
    val rootPath = new File(file).getParent
    val (root, map) = parseHTMLFromReader(file, new FileReader(file))
    T(rootPath = rootPath, name = file, root = root, spans = map)
  }
  override def parseHTMLFromString(html: String, file: String): T = {
    val (root, map) = parseHTMLFromReader(file, new StringReader(html))
    T(rootPath = null, name = file, root = root, spans = map)
  }
  override def parseFragment(markup: String): Node = {
    parseHTMLFragmentFromReader(new StringReader(markup))
  }

  private def parseHTMLFromReader(file: String, r: Reader): (Document, HashMap[Node, (String, Int, Int)]) = {
    val parser: DOMParser = new DOMParser
    parser.setFeature("http://xml.org/sax/features/namespaces", false)
    parser.setFeature(AUGMENTATIONS, true)
    val list = new Array[XMLDocumentFilter](1)

    var lineList = List.empty[(String, Int, Int)]
    list.update(0, new DefaultFilter {
      override def startElement(element: QName, attributes: XMLAttributes, augs: Augmentations): Unit = {
        val o = augs.getItem("http://cyberneko.org/html/features/augmentations").asInstanceOf[LocationItem]
        lineList ::= (element.localpart, o.getEndLineNumber, o.getEndCharacterOffset + 1)
        super.startElement(element, attributes, augs)
      }

      override def emptyElement(element: QName, attributes: XMLAttributes, augs: Augmentations): Unit = {
        val o = augs.getItem("http://cyberneko.org/html/features/augmentations").asInstanceOf[LocationItem]
        lineList ::= (element.localpart, o.getEndLineNumber, o.getEndCharacterOffset + 1)
        super.startElement(element, attributes, augs)
      }
    })
    parser.setProperty("http://cyberneko.org/html/properties/filters", list)
    parser.parse(new InputSource(r))

    val doc = parser.getDocument
    var lines = lineList.reverse
    def matching(n: Node, map: HashMap[Node, (String, Int, Int)]): HashMap[Node, (String, Int, Int)] = {
      if (n == null) map
      else {
        val lname = n.getNodeName
        val nmap_1 =
          (lname, lines.headOption) match {
            case (n1, Some((n2, _, _))) if n1 == n2 =>
              val nmap = map + (n -> lines.head.copy(_1 = file))
              lines = lines.tail
              nmap
            case _ =>
              map
          }
        val nmap_2 = matching(n.getFirstChild, nmap_1)
        matching(n.getNextSibling, nmap_2)
      }
    }
    val lineMap = matching(doc.getFirstChild, HashMap.empty)
    (doc, lineMap)
  }
  private def parseHTMLFragmentFromReader(r: Reader): Node = {
    val parser: DOMFragmentParser = new DOMFragmentParser()
    val document: HTMLDocument = new HTMLDocumentImpl()
    val fragment: DocumentFragment = document.createDocumentFragment()
    parser.parse(new InputSource(r), fragment)

    fragment
  }

  override def createTextNode(s: String): Node = {
    val document = new HTMLDocumentImpl()
    document.createTextNode(s)
  }
  override def createComment(s: String): Node = {
    val document = new HTMLDocumentImpl()
    document.createComment(s)
  }
  override def extractScript(t: T): List[CodeFragment] = {
    val name = t.name
    val root = t.root
    val spans = t.spans
    val nlist = root.getElementsByTagName("SCRIPT")
    (0 until nlist.getLength).foldRight[List[CodeFragment]](Nil)((i, list) => {
      val node = nlist.item(i)
      val span = spans(node)
      val script = node.asInstanceOf[Element]
      val tval = script.getAttribute("type")
      val src = script.getAttribute("src")

      if (tval.isEmpty || types.exists(tval.equalsIgnoreCase)) {
        if (src.isEmpty) {
          val c: CodeFragment = CodeFragment(span._1, span._2, span._3, node.getTextContent)
          c :: list
        } else {
          try {
            readContent(t)(src) :: list
          } catch {
            case e: InternalError =>
              System.err.println("* Warning: " + e.getMessage)
              list
          }
        }
      } else {
        list
      }
    })
  }
  override def getRootPath(t: T): String = t.rootPath
  override def toDOM(t: T): Document = t.root
  override def getOptionalSpan(t: T)(n: Node): Option[(String, Int, Int)] = {
    t.spans.get(n) match {
      case Some(s) => Some(s)
      case None =>
        n.getNodeName match {
          case "#text" | "#comment" => None
          case _ =>
            System.out.println(s"* Warning: cannot find location information for node ${n.getNodeName}")
            None
        }
    }
  }
}
