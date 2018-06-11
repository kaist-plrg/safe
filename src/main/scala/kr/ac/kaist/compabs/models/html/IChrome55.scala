/**
 * *****************************************************************************
 * Copyright (c) 2015-2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.html

import kr.ac.kaist.compabs.models.html.parser.{ CybernekoParser, HTMLParser }
import kr.ac.kaist.compabs.models.shape.Host
import kr.ac.kaist.safe.util.NodeUtil.{ INTERNAL_BOOL_TOP, INTERNAL_CALL, INTERNAL_EVENT_FUNC }
import org.w3c.dom._
import org.w3c.dom.html._
import kr.ac.kaist.compabs.models._

import scala.collection.immutable.{ HashMap, HashSet }
import scala.collection.mutable
import scala.language.reflectiveCalls

trait IChrome55 extends DOMModel {
  val name: String

  override val Parser: HTMLParser = CybernekoParser
  val appCodeName: String = "Mozilla"
  val appName: String = "Netscape"
  val appVersion: String = "5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.95 Safari/537.36"
  val userAgent: String = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.95 Safari/537.36"
  val compatMode: String = "CSS1Compat"
  val platform: String = "MacIntel" // "MacIntel", "Win32", "FreeBSD i386", "WebTV OS"
  val product: String = "Gecko"

  // wrapper type
  type SemanticsFun = (AAbsValue, AAbsState) => (AAbsState, AAbsState, AAbsValue)
  trait SFInputT {
    def getArg(i: Int): AAbsValue
    def lenArg: AAbsNum
  }
  type SFInput <: SFInputT

  def genAPI(i: SFInput => (AAbsState, AAbsState, AAbsValue)): SemanticsFun
  def nnAPI(name: String): SemanticsFun
  def returnValueA(v: AAbsBool)(s: SFInput): (AAbsState, AAbsState, AAbsValue)
  def returnValueB(v: AAbsStr)(s: SFInput): (AAbsState, AAbsState, AAbsValue)
  def returnValueC(v: AAbsValue)(s: SFInput): (AAbsState, AAbsState, AAbsValue)
  def returnValueD(v: AAbsValue, es: AAbsState)(s: SFInput): (AAbsState, AAbsState, AAbsValue)
  def returnValueAPI(v: AAbsValue): SemanticsFun
  def returnValueAPI(b: Boolean): SemanticsFun
  def returnValueAPI(s: String): SemanticsFun
  def returnValueLocName(s: String): SemanticsFun
  def returnValueIRange(l: Int, u: Int): SemanticsFun
  def returnUndef(si: SFInput): (AAbsState, AAbsState, AAbsValue)
  def toDOMString(v: AAbsValue): HashSet[String]

  def exception(es: Set[AException])(s: SFInput): AAbsState

  // Note: Exceptional case: "float" -> "cssFloat"
  val cssTocssom: HashMap[String, String] = HashMap("background" -> "background", "background-attachment" -> "backgroundAttachment", "background-color" -> "backgroundColor",
    "background-image" -> "backgroundImage", "background-position" -> "backgroundPosition", "background-repeat" -> "backgroundRepeat", "border" -> "border",
    "border-bottom" -> "borderBottom", "border-bottom-color" -> "borderBottomColor", "border-bottom-style" -> "borderBottomStyle", "border-bottom-width" -> "borderBottomWidth",
    "border-color" -> "borderColor", "border-left" -> "borderLeft", "border-left-color" -> "borderLeftColor", "border-left-style" -> "borderLeftStyle",
    "border-left-width" -> "borderLeftWidth", "border-right" -> "borderRight", "border-right-color" -> "borderRightColor", "border-right-style" -> "borderRightStyle",
    "border-right-width" -> "borderRightWidth", "border-style" -> "borderStyle", "border-top" -> "borderTop", "border-top-color" -> "borderTopColor",
    "border-top-style" -> "borderTopStyle", "border-top-width" -> "borderTopWidth", "border-width" -> "borderWidth", "clear" -> "clear", "clip" -> "clip",
    "color" -> "color", "cursor" -> "cursor", "display" -> "display", "filter" -> "filter", "font" -> "font", "font-family" -> "fontFamily", "font-size" -> "fontSize",
    "font-variant" -> "fontVariant", "font-weight" -> "fontWeight", "height" -> "height", "left" -> "left", "letter-spacing" -> "letterSpacing", "line-height" -> "lineHeight",
    "list-style" -> "listStyle", "list-style-image" -> "listStyleImage", "list-style-position" -> "listStylePosition", "list-style-type" -> "listStyleType",
    "margin" -> "margin", "margin-bottom" -> "marginBottom", "margin-left" -> "marginLeft", "margin-right" -> "marginRight", "margin-top" -> "marginTop", "overflow" -> "overflow",
    "opacity" -> "opacity", "padding" -> "padding", "padding-bottom" -> "paddingBottom", "padding-left" -> "paddingLeft", "padding-right" -> "paddingRight", "padding-top" -> "paddingTop",
    "page-break-after" -> "pageBreakAfter", "page-break-before" -> "pageBreakBefore", "position" -> "position", "float" -> "cssFloat", "text-align" -> "textAlign",
    "text-decoration" -> "textDecoration", "text-indent" -> "textIndent", "text-transform" -> "textTransform", "top" -> "top", "vertical-align" -> "verticalAlign",
    "visibility" -> "visibility", "width" -> "width", "zoom" -> "zoom", "z-index" -> "zIndex")

  val eventHandlers: HashSet[String] = HashSet("onclick", "onkeydown", "onload", "onfocus")
  private[this] def isEventHandler(name: String): Boolean = eventHandlers.contains(name)

  var owner: Document = _
  var htmlDoc: Option[AAbsValue] = None
  var htmlHTML: Option[AAbsValue] = None
  var htmlHead: Option[AAbsValue] = None
  var htmlTitle: Option[AAbsValue] = None
  var htmlBody: Option[AAbsValue] = None

  var uid: Int = 0
  def uidGen(name: String): String = {
    val v = uid
    val id = s"#_$name.@$v"
    uid = uid + 1
    id
  }

  private def nodeClass(node: Node): String = {
    node match {
      case _: Attr => "Attr"
      case _: CDATASection => "CDATASection"
      case _: Comment => "Comment"
      case _: DocumentFragment => "DocumentFragment"
      case _: DocumentType => "DocumentType"
      case _: Text => "Text"
      case _: HTMLAnchorElement => "HTMLAnchorElement"
      case _: HTMLAppletElement => "HTMLAppletElement"
      case _: HTMLAreaElement => "HTMLAreaElement"
      case _: HTMLBaseElement => "HTMLBaseElement"
      case _: HTMLBaseFontElement => "HTMLBaseFontElement"
      case _: HTMLBodyElement => "HTMLBodyElement"
      case _: HTMLBRElement => "HTMLBRElement"
      case _: HTMLButtonElement => "HTMLButtonElement"
      case _: HTMLCollection => "HTMLCollection"
      case _: HTMLDirectoryElement => "HTMLDirectoryElement"
      case _: HTMLDivElement => "HTMLDivElement"
      case _: HTMLDListElement => "HTMLDListElement"
      case _: HTMLDocument => "HTMLDocument"
      case _: HTMLDOMImplementation => "HTMLDOMImplementation"
      case _: HTMLFieldSetElement => "HTMLFieldSetElement"
      case _: HTMLFontElement => "HTMLFontElement"
      case _: HTMLFormElement => "HTMLFormElement"
      case _: HTMLFrameElement => "HTMLFrameElement"
      case _: HTMLFrameSetElement => "HTMLFrameSetElement"
      case _: HTMLHeadElement => "HTMLHeadElement"
      case _: HTMLHeadingElement => "HTMLHeadingElement"
      case _: HTMLHRElement => "HTMLHRElement"
      case _: HTMLHtmlElement => "HTMLHtmlElement"
      case _: HTMLIFrameElement => "HTMLIFrameElement"
      case _: HTMLImageElement => "HTMLImageElement"
      case _: HTMLInputElement => "HTMLInputElement"
      case _: HTMLIsIndexElement => "HTMLIsIndexElement"
      case _: HTMLLabelElement => "HTMLLabelElement"
      case _: HTMLLegendElement => "HTMLLegendElement"
      case _: HTMLLIElement => "HTMLLIElement"
      case _: HTMLLinkElement => "HTMLLinkElement"
      case _: HTMLMapElement => "HTMLMapElement"
      case _: HTMLMenuElement => "HTMLMenuElement"
      case _: HTMLMetaElement => "HTMLMetaElement"
      case _: HTMLModElement => "HTMLModElement"
      case _: HTMLObjectElement => "HTMLObjectElement"
      case _: HTMLOListElement => "HTMLOListElement"
      case _: HTMLOptGroupElement => "HTMLOptGroupElement"
      case _: HTMLOptionElement => "HTMLOptionElement"
      case _: HTMLParagraphElement => "HTMLParagraphElement"
      case _: HTMLParamElement => "HTMLParamElement"
      case _: HTMLPreElement => "HTMLPreElement"
      case _: HTMLQuoteElement => "HTMLQuoteElement"
      case _: HTMLScriptElement => "HTMLScriptElement"
      case _: HTMLSelectElement => "HTMLSelectElement"
      case _: HTMLStyleElement => "HTMLStyleElement"
      case _: HTMLTableCaptionElement => "HTMLTableCaptionElement"
      case _: HTMLTableCellElement => "HTMLTableCellElement"
      case _: HTMLTableColElement => "HTMLTableColElement"
      case _: HTMLTableElement => "HTMLTableElement"
      case _: HTMLTableRowElement => "HTMLTableRowElement"
      case _: HTMLTableSectionElement => "HTMLTableSectionElement"
      case _: HTMLTextAreaElement => "HTMLTextAreaElement"
      case _: HTMLTitleElement => "HTMLTitleElement"
      case _: HTMLUListElement => "HTMLUListElement"
      case _: HTMLElement => "HTMLElement"
      case _ =>
        System.err.println(s"* Warning: an unknown element: ${node.getNodeName} - ${node.getClass.toString}")
        "HTMLUnknownElement"
    }
  }

  val mapID: mutable.AnyRefMap[Node, String] = mutable.AnyRefMap[Node, String]()

  def getID(node: Node, name: String = null, dynamic: Boolean = false): String = {
    if (node == null) null
    else {
      if (name != null) mapID.getOrElseUpdate(node, name)
      else {
        lazy val v =
          if (dynamic) node |> nodeClass
          else node |> nodeClass |> uidGen
        mapID.getOrElseUpdate(node, v)
      }
    }
  }

  def warning(message: String): SemanticsFun = {
    genAPI(si => {
      System.err.println(s"* Warning: $message")
      returnUndef(si)
    })
  }

  case class T(dom: Parser.T)

  def parseHTMLFromFile(name: String): T = {
    T(Parser.parseHTMLFromFile(name))
  }

  def loadModel: Host = {
    Host.load(path, name)
  }

  def getProgram(t: T): List[CodeFragment] = {
    // Extract JavaScript code from HTML. Not event handlers.
    val codes = Parser.extractScript(t.dom)

    val evt = INTERNAL_EVENT_FUNC
    // Build a CFG that includes an event handling loop.
    val event_body = CodeFragment("#event#loop", 0, 0, s"while($INTERNAL_BOOL_TOP) { $INTERNAL_CALL($evt.func, $evt.elem, []); }")
    codes :+ event_body
  }

  def isSupported: String => Boolean = Parser.isSupported

  def genSemantics(): String => (AAbsValue, AAbsState) => (AAbsState, AAbsState, AAbsValue) = {
    val ssSemantics: Map[String, SemanticsFun] = HashMap(
      "console.log" -> genAPI(si => {
        System.err.println(s"- console.log: '${si.getArg(0)}'")
        returnUndef(si)
      }),
      "chrome.app.getIsInstalled" -> returnValueAPI(false),

      "Navigator.prototype.appCodeName$get" -> returnValueAPI(appCodeName),
      "Navigator.prototype.appName$get" -> returnValueAPI(appName),
      "Navigator.prototype.appVersion$get" -> returnValueAPI(appVersion),
      "Navigator.prototype.userAgent$get" -> returnValueAPI(userAgent),
      "Navigator.prototype.platform$get" -> returnValueAPI(platform),
      "Navigator.prototype.product$get" -> returnValueAPI(product),

      //      "Screen.prototype.colorDepth$get" -> returnValueAPI(Value(AbsNum)),
      //      "Screen.prototype.height$get" -> returnConstant(Value(AbsNumber.naturalNumbers)),
      //      "Screen.prototype.width$get" -> returnConstant(Value(AbsNumber.naturalNumbers)),

      "document.location$get" -> returnValueLocName("#location"),

      "Node.prototype.compareDocumentPosition" -> returnValueIRange(0, 64)

    )
    name: String => ssSemantics.getOrElse(name.substring(1), nnAPI(name))
  }

  //  val document = Parser.toDOM(t.dom)
  //  val spans = Parser.getOptionalSpan(t.dom)(_)

}