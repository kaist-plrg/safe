/**
 * *****************************************************************************
 * Copyright (c) 2015-2017, KAIST.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
package kr.ac.kaist.compabs.models.html

import org.w3c.dom._
import org.w3c.dom.html._

import scala.collection.immutable.{ HashMap, HashSet }
import scala.collection.mutable
import scala.language.reflectiveCalls
import kr.ac.kaist.compabs.models.html.parser.{ CybernekoParser, HTMLParser }
import kr.ac.kaist.compabs.models.shape.Host
import kr.ac.kaist.compabs.models._
import kr.ac.kaist.compabs.models.cdomain.CValue
import kr.ac.kaist.safe.html.Chrome55.{ AAbsIValue, AAbsObj, AAbsStr, AAbsValue }

// TODO
import kr.ac.kaist.safe.analyzer.domain._

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
    def copyi(state: AAbsState): SFInput
    val state: AAbsState
    def getArg(i: Int): AAbsValue
    def lenArg: AAbsNum
    def newAlloc(j: Long): ALoc
    def newRecentAlloc(j: Long): ALoc
    def newDAlloc(j: Long): DLoc.T
    val lset_this: AAbsLoc
    val v_this: AAbsValue
    val id: Long
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
  val docName: String = "#document"
  val docHTMLName: String = "#documentHTML"
  val docHeadName: String = "#documentHead"
  val docTitleName: String = "#documentTitle"
  val docBodyName: String = "#documentBody"

  sealed trait NamedNode
  case object DocumentNode extends NamedNode
  case object DocumentElementNode extends NamedNode
  case object HeadNode extends NamedNode
  case object BodyNode extends NamedNode
  case object TitleNode extends NamedNode

  val AAbsValueNull: AAbsValue

  def getNamedNode(name: NamedNode): AAbsValue = {
    lazy val nt = AAbsValueNull
    name match {
      case DocumentNode => htmlDoc.getOrElse(nt)
      case DocumentElementNode => htmlHTML.getOrElse(nt)
      case HeadNode => htmlHead.getOrElse(nt)
      case TitleNode => htmlTitle.getOrElse(nt)
      case BodyNode => htmlBody.getOrElse(nt)
    }
  }

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
  def toDOM(t: T): Document = Parser.toDOM(t.dom)

  def loadModel: Host = {
    Host.load(path, name)
  }

  def getProgram(t: T): List[CodeFragment] = {
    // Extract JavaScript code from HTML. Not event handlers.
    Parser.extractScript(t.dom)
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
      "Navigator.prototype.cookieEnabled$get" -> returnValueAPI(true),
      "Navigator.prototype.language$get" -> returnValueAPI("ko-KR"),

      "Screen.prototype.colorDepth$get" -> returnValueAPI(NtoValue(UInt)),
      "Screen.prototype.height$get" -> returnValueAPI(NtoValue(UInt)),
      "Screen.prototype.width$get" -> returnValueAPI(NtoValue(UInt)),

      "History.prototype.length$get" -> returnValueAPI(NtoValue(UInt)),

      "document.location$get" -> returnValueLocName("#location"),
      "document.location$set" ->
        genAPI(si => {
          System.err.println("Ignored attempts to update the value of 'document.location' property")
          returnUndef(si)
        }),

      "Node.prototype.compareDocumentPosition" -> returnValueIRange(0, 64),

      "setInterval" -> genAPI(si => {
        val s: AAbsState = si.state
        val handler: AAbsValue = si.getArg(0)
        val handlers = filterCallable(s, handler)

        assert(isNonEmpty(handlers))

        val addr = si.newAlloc(1)
        val (v, s_n) = setInterval(handlers, addr)(s)
        returnValueC(v)(si.copyi(state = s_n))
      }),

      "Document.prototype.createElement" -> genAPI(si => {
        val name = callToString(si)(si.getArg(0))
        val v_doc = si.v_this

        val (v, s_n) =
          ((ValueBot, StateBot) /: name)((s_i, n) => {
            val (v_i, s_n) =
              gamma_str(n) match {
                case Some(strs) if strs.size == 1 =>
                  createElement(strs.head, v_doc)(si)
                case _ =>
                  System.err.println(s"* Warning: ignore abstract tag name case: $n")
                  createElement("DIV", v_doc)(si)
              }
            (joinValue(s_i._1, v_i), joinState(s_i._2, s_n))
          })

        returnValueC(v)(si.copyi(state = s_n))
      })

    )
    name: String => ssSemantics.getOrElse(name.substring(1), nnAPI(name))
  }

  def joinValue(v1: AAbsValue, v2: AAbsValue): AAbsValue
  def joinState(v1: AAbsState, v2: AAbsState): AAbsState

  def createElement(name: String, v_doc: AAbsValue)(s: SFInput): (AAbsValue, AAbsState) = {
    val tagName = name.toLowerCase
    val addr = s.newRecentAlloc(1)
    val a_style = s.newRecentAlloc(2)
    val a_childNodes = s.newRecentAlloc(3)
    val a_attr = s.newRecentAlloc(4)

    // TODO
    val ii = -1 // addr.toLong
    val st = s.state

    val s_0 = st |> oldify(addr) |> oldify(a_style) |> oldify(a_childNodes) |> oldify(a_attr)
    val s_1 = doldify(ii)(s_0)

    val l = addr
    val l_style = a_style
    val l_childNodes = a_childNodes
    val l_attr = a_attr
    val dl = DLoc.recent(ii)

    //    mustNew(s_1, l)
    //    mustNew(s_1, l_style)
    //    mustNew(s_1, l_childNodes)
    //    mustNew(s_1, l_attr)
    //    assert(!s_1.ddomIn(dl))

    val node = owner.createElement(tagName)
    val cls = nodeClass(node)
    val l_proto = sysLoc(s"#$cls.prototype")
    // links between DOM node and JS obj.
    val o =
      newObject(l_proto, cls, bExt = BoolTrue) |>
        update(INode, toIValue(dl)) |>
        update(IStyle, toIValue(LtoValue(l_style))) |>
        update(IChildNodes, toIValue(LtoValue(l_childNodes))) |>
        update(IAttributes, toIValue(LtoValue(l_attr)))

    val d = newDObject |> dupdate("obj", LtoValue(l))
    val (o_2, d_2) = updateProps(cls, node, v_doc)(o, d)
    val o_style = update(IStyle, toIValue(LtoValue(l_style)))(o_style_default)

    val o_n = newArrayObject(toAbsNum(0))

    // TODO children
    val s_2 = s_1 |>
      supdate(l_style, o_style) |>
      supdate(l, o_2) |>
      supdate(l_childNodes, emptyNodeList) |>
      supdate(l_attr, o_n)
    val s_3 = dupdate(dl, d_2)(s_2)

    (LtoValue(l), s_3)
  }

  def updateDocument(doc: Node, s: AAbsState): AAbsState = {
    owner = doc.asInstanceOf[Document]
    val id = getID(doc)
    val l_childNodes = sysLoc(id + "_childNodes")
    val l = sysLoc(id)
    val dl = DLoc.sys(id)

    val o = s |> slookup(l) |> update(INode, toIValue(dl)) |>
      update(IChildNodes, toIValue(LtoValue(l_childNodes)))
    val d = newDocument |> dupdate("obj", LtoValue(l))

    // TODO initialize attributes.
    val (o_2, d_2) = updateProps(nodeClass(doc), doc, AAbsValueNull)(o, d)
    // TODO event handlers

    s |>
      supdate(l, o_2) |>
      supdate(l_childNodes, emptyNodeList) |>
      dupdate(dl, d_2)
  }

  def mustNew(s: AAbsState, l: ALoc): Unit = {
    assert(!(s |> sdomIn(l)), s"$l already exist:\n") // ${DomainPrinter.printObj(2, s(l))}")
  }

  def createElementA(node: Node, v_doc: AAbsValue, dynamic: Boolean = false, span: Option[(String, Int, Int)] = None)(s: AAbsState, alloc: String => ALoc, dalloc: String => DLoc.T, parseFunction: FuncParser): (ALoc, AAbsState) = {
    val id = getID(node, dynamic = dynamic)
    val l = alloc(id)
    val dl = dalloc(id)
    val l_style = alloc(id + "_style")
    val l_childNodes = alloc(id + "_childNodes")
    val l_attr = alloc(id + "_attr")

    val s_0 = s |> oldify(l) |> oldify(l_style) |> oldify(l_childNodes) |> oldify(l_attr)
    val s_1 = s_0 |> doldify(dl.allocSite)

    mustNew(s_1, l)
    mustNew(s_1, l_style)
    mustNew(s_1, l_childNodes)
    mustNew(s_1, l_attr)

    val cls = nodeClass(node)
    val l_proto = sysLoc(s"#$cls.prototype")

    val o =
      newObject(l_proto, cls) |>
        update(INode, toIValue(dl)) |>
        update(IStyle, toIValue(LtoValue(l_style))) |>
        update(IChildNodes, toIValue(LtoValue(l_childNodes))) |>
        update(IAttributes, toIValue(LtoValue(l_attr)))

    val o_n =
      newArrayObject(toAbsNum(0))

    val d = newDObject |> dupdate("obj", LtoValue(l))
    val (o_2, d_2) = updateProps(cls, node, v_doc)(o, d)

    // event handlers
    val attrs = node.getAttributes
    val list =
      (List.empty[(String, ALoc, AAbsObj)] /: (0 until attrs.getLength))((d_i, i) => {
        val a = attrs.item(i).asInstanceOf[Attr]
        val fn = a.getName

        if (isEventHandler(fn)) {
          val name = fn.substring(2)
          val body = a.getValue
          val o = parseFunction(s"function (event) { $body }", span)
          val l_cb = alloc(s"${id}_$name")
          (name, l_cb, o) :: d_i
        } else if (fn.startsWith("on")) {
          System.err.println(s"* Warning: Suspicious attribute name: $fn")
          d_i
        } else d_i
      })
    val s_2 = (s_1 /: list)((s_i, nlo) => {
      mustNew(s_i, nlo._2)
      s_i |> supdate(nlo._2, nlo._3)
    })
    val d_3 = d_2 // TODO: (d_2 /: list) ((d_i, nlo) => d_i.addEventListener(nlo._1, nlo._2, bubble = false))

    val o_style = o_style_default |> update(IStyle, toIValue(LtoValue(l_style)))
    val style = attr(node)("style")

    val o_style_2 =
      if (style == null || style.equals("")) o_style
      else o_style
    // TODO
    //      {
    //        val css = parseInlineCSS(style)
    //        (o_style /: (0 until css.getLength)) ((o_i, i) => {
    //          val key = css.item(i)
    //          val value = css.getPropertyValue(key)
    //
    //          cssTocssom.get(key) match {
    //            case Some(nkey) =>
    //              // TODO key and nkey must be binded. We can support this feature by getter/setter.
    //              if (nkey.equalsIgnoreCase(key))
    //                o_i |> oupdate(key, value)
    //              else
    //                o_i |> oupdate(key, value) |> oupdate(nkey, value)
    //            case _ =>
    //              System.err.println(s"* Warning: Ignored style sheet property: $key")
    //              o_i
    //          }
    //        })
    //      }

    // TODO children
    val s_3 = s_2 |> supdate(l_style, o_style_2) |>
      supdate(l_childNodes, emptyNodeList) |>
      supdate(l, o_2) |>
      supdate(l_attr, o_n)

    val s_4 = s_3 |> dupdate(dl, d_3)

    (l, s_4)
  }

  def createNode(node: Node, v_doc: AAbsValue, dynamic: Boolean = false, span: Option[(String, Int, Int)] = None)(s: AAbsState, alloc: String => ALoc, dalloc: String => DLoc.T, parseFunction: FuncParser): (ALoc, AAbsState) = {
    if (node.isInstanceOf[Element]) createElementA(node, v_doc, dynamic, span)(s, alloc, dalloc, parseFunction)
    else {
      val id = getID(node, dynamic = dynamic)

      val l = alloc(id)
      val dl = dalloc(id)
      val l_childNodes = alloc(id + "_childNodes")

      val s_0 = s |> oldify(l) |> oldify(l_childNodes)
      val s_1 = s_0 |> doldify(dl.allocSite)

      mustNew(s_1, l)
      mustNew(s_1, l_childNodes)

      val cls = nodeClass(node)
      val l_proto = sysLoc(s"#$cls.prototype")
      val o =
        newObject(l_proto, cls) |>
          update(INode, toIValue(dl)) |>
          update(IChildNodes, toIValue(LtoValue(l_childNodes)))
      val d = newDObject |> dupdate("obj", LtoValue(l))
      val (o_2, d_2) = updateProps(cls, node, v_doc)(o, d)

      // TODO event handlers
      // TODO children
      val s_2 = s_1 |> supdate(l, o_2) |> supdate(l_childNodes, emptyNodeList)
      val s_3 = s_2 |> dupdate(dl, d_2)

      (l, s_3)
    }
  }

  def lastSiblings(inode: DLoc.T)(s: AAbsState): DLocSet.T = {
    var visited = DLocSet.bottom

    def collects(icurr: DLoc.T, lasts: DLocSet.T): DLocSet.T = {
      if (visited.contains(icurr)) lasts
      else {
        visited = visited + icurr
        val nexts: DLocSet.T = nextSiblings(icurr)(s)
        (lasts /: nexts)((lasts_i, next) => {
          if (next == DLoc.nullv) lasts_i + icurr
          else collects(next, lasts_i)
        })
      }
    }

    collects(inode, DLocSet.bottom)
  }

  def firstChilds(inode: DLoc.T)(s: AAbsState): DLocSet.T = s |> dlookup(inode) |> ddlookup("firstChild") |> dlocset

  def nextSiblings(inode: DLoc.T)(s: AAbsState): DLocSet.T = s |> dlookup(inode) |> ddlookup("nextSibling") |> dlocset

  def parentNodes(inode: DLoc.T)(s: AAbsState): DLocSet.T = s |> dlookup(inode) |> ddlookup("parentNode") |> dlocset

  private def appendChildi(iparent: DLoc.T, inode: DLoc.T)(s: AAbsState): AAbsState = {
    val lset_parent = s |> dlookup(iparent) |> ddlookup("obj")
    val firsts = firstChilds(iparent)(s)

    val firsts_n = firsts.hasNull
    val ilasts = (DLocSet.bottom /: firsts.nonNulls)((locs_i, first) => locs_i + lastSiblings(first)(s))
    val node = s |> dlookup(inode) |> dupdate("parentNode", DtoValue(iparent)) |> dupdate("nextSibling", DtoValue(DLoc.nullv))

    val s_last =
      (StateBot /: ilasts)((s_i, ilast) => {
        val last = s |> dlookup(ilast)
        val nlast = last |> dupdate("nextSibling", DtoValue(inode))
        joinState(s_i, s |> dupdate(ilast, nlast) |> dupdate(inode, node))
      })

    val s_first =
      if (firsts_n) {
        val o = s |> dlookup(iparent)
        s |> dupdate(iparent, o |> dupdate("firstChild", DtoValue(inode))) |> dupdate(inode, node)
      } else StateBot

    val s_n = joinState(s_first, s_last)

    val lset_childNodes = lsetFoldLeft(lset_parent)(ValueBot)((lset_i, l) => {
      joinValue(lset_i, s |> slookup(l) |> lookupi(IChildNodes) |> ItoValue)
    })
    val lset_nodes = s |> dlookup(inode) |> ddlookup("obj")
    lsetFoldLeft(lset_childNodes)(StateBot)((s_i, l) => joinState(s_i, appendLast(l, lset_nodes)(s_n)))
  }

  private def appendLast(l_childNodes: ALoc, lset_nodes: AAbsValue)(s: AAbsState): AAbsState = {
    val o = s |> slookup(l_childNodes)
    val length = o |> lookupi(ILength)
    length |> ItoValue |> NgetSingle match {
      case Some(i) =>
        val n_length = i + 1
        val o_n =
          o |> update(ILength, toIValue(NtoValue(toAbsNum(n_length)))) |>
            update(i.toInt.toString, toDataProp(lset_nodes))
        s |> supdate(l_childNodes, o_n)
      case _ =>
        val v_len = UInt
        val lset = o |> lookup(IntStr)
        val lset_n = joinValue(lset, lset_nodes)
        val o_n =
          o |> update(ILength, toIValue(NtoValue(v_len))) |>
            update(IntStr, toDataProp(lset_n))
        s |> supdate(l_childNodes, o_n)
    }
  }

  def lsetFoldLeft[T](v: AAbsValue)(i: T)(f: (T, ALoc) => T): T

  def appendChild(parent: Node, node: Node)(s: AAbsState): AAbsState = {
    val iparent = DLoc.sys(getID(parent))
    val inode = DLoc.sys(getID(node))
    appendChildi(iparent, inode)(s)
  }

  def genInitialDOMTree(node: Document, getSpan: Node => Option[(String, Int, Int)], s: AAbsState, parseFunction: FuncParser): AAbsState = {
    def toValue(s: String): Option[AAbsValue] = Some(LtoValue(sysLoc(s)))

    val global = GlobalLoc
    val dglobal = DLoc.sys("#Global")
    val s_1 =
      s |>
        supdate(global, slookup(global)(s) |> update(INode, toIValue(dglobal))) |>
        dupdate(dglobal, newDObject |> dupdate("obj", LtoValue(global)))

    val documentID: String = getID(node, docName)
    htmlDoc = toValue(documentID)
    val s_2 = updateDocument(node, s_1)
    val v_doc = LtoValue(sysLoc(getID(node)))

    def tree(node: Node, parent: Node)(s_i: AAbsState): AAbsState = {
      if (node == null) s_i
      else {
        nodeClass(node) match {
          case "HTMLHtmlElement" if htmlHTML.isEmpty => htmlHTML = toValue(getID(node, docHTMLName))
          case "HTMLHeadElement" if htmlHead.isEmpty => htmlHead = toValue(getID(node, docHeadName))
          case "HTMLTitleElement" if htmlTitle.isEmpty => htmlTitle = toValue(getID(node, docTitleName))
          case "HTMLBodyElement" if htmlBody.isEmpty => htmlBody = toValue(getID(node, docBodyName))
          case _ => // do nothing
        }

        def getAddr(id: String): (ALoc, DLoc.T) = {
          val l = sysLoc(id)
          val dl = DLoc.sys(id)
          (l, dl)
        }

        def Alloc(id: String): ALoc = sysLoc(id)

        def DAlloc(id: String): DLoc.T = DLoc.sys(id)

        createNode(node, v_doc, span = getSpan(node))(s_i, Alloc, DAlloc, parseFunction)._2 |>
          appendChild(parent, node) |>
          tree(node.getFirstChild, node) |>
          tree(node.getNextSibling, parent)
      }
    }

    tree(node.getFirstChild, node)(s_2)
  }

  def sdomIn(l: ALoc)(s: AAbsState): Boolean
  def slookup(l: ALoc)(s: AAbsState): AAbsObj
  def dupdate(dl: DLoc.T, od: AAbsDObject)(s: AAbsState): AAbsState
  def supdate(l: ALoc, o: AAbsObj)(s: AAbsState): AAbsState
  def toAbsNum(i: Double): AAbsNum
  def toDataProp(v: AAbsValue): AAbsDataProp

  private[this] def oupdate(key: String, value: String)(o: AAbsObj): AAbsObj =
    o |> update(key, toDataProp(StoValue(toAbsStr(value))))

  private[this] def uupdate(key: String)(o: AAbsObj): AAbsObj =
    o |> update(key, toDataProp(StoValue(OtherStr)))

  lazy val emptyNodeList: AAbsObj = {
    val l_proto = sysLoc(s"#NodeList.prototype")
    newObject(l_proto, "NodeList") |> update(ILength, toIValue(NtoValue(toAbsNum(0))))
  }

  lazy val o_style_default: AAbsObj = {
    val l_style_proto = sysLoc(s"#CSSStyleDeclaration.prototype")
    val o_style = newObject(l_style_proto, "CSSStyleDeclaration")

    (o_style /: cssTocssom)((o_style_i, kv) => o_style_i |> oupdate(kv._1, "") |> oupdate(kv._2, ""))
  }
  lazy val o_style_computed_default: AAbsObj = {
    val l_style_proto = sysLoc(s"#CSSStyleDeclaration.prototype")
    val o_style = newObject(l_style_proto, "CSSStyleDeclaration")

    (o_style /: cssTocssom)((o_style_i, kv) => o_style_i |> uupdate(kv._1) |> uupdate(kv._2))
  }

  val newDocument: AAbsDObject

  def attrs(node: Node): HashMap[String, String] = {
    val map = node.getAttributes
    val length = map.getLength
    (HashMap.empty[String, String] /: (0 until length)) {
      case (m_i, i) =>
        val ith = map.item(i)
        val name = ith.getNodeName
        val value = ith.getNodeValue
        if (isEventHandler(name)) m_i
        else m_i + (name -> value)
    }
  }

  def attr(node: Node)(prop: String): String = {
    val map = node.getAttributes
    if (map == null) null
    else {
      val n = map.getNamedItem(prop)
      val attr = n.asInstanceOf[Attr]
      if (attr == null) ""
      else attr.getValue
    }
  }

  def updateIProp(name: String, value: CValue)(od: (AAbsObj, AAbsDObject)): (AAbsObj, AAbsDObject)
  def updateIProp(name: String, value: AbsValue)(od: (AAbsObj, AAbsDObject)): (AAbsObj, AAbsDObject)
  def updateProp(name: String, value: CValue)(od: (AAbsObj, AAbsDObject)): (AAbsObj, AAbsDObject)
  def updateProp(name: String, value: AbsValue)(od: (AAbsObj, AAbsDObject)): (AAbsObj, AAbsDObject)

  def updateProps(name: String, node: Node, v_doc: AAbsValue)(o: AAbsObj, d: AAbsDObject): (AAbsObj, AAbsDObject)

  def dlocset(v: AAbsValue): DLocSet.T
  def locset(v: AAbsIValue): AbsLoc
  def ddlookup(s: String)(o: AAbsDObject): AAbsValue
  def dlookup(l: DLoc.T)(s: AAbsState): AAbsDObject
  def lookup(s: AAbsStr)(o: AAbsObj): AAbsValue
  def lookupi(n: IName)(o: AAbsObj): AAbsIValue
  def dupdate(name: String, v: AAbsValue)(o: AAbsDObject): AAbsDObject
  def update(name: IName, v: AAbsIValue)(o: AAbsObj): AAbsObj
  def update(name: String, v: AAbsDataProp)(o: AAbsObj): AAbsObj
  def update(name: AAbsStr, v: AAbsDataProp)(o: AAbsObj): AAbsObj
  def toAbsStr(s: String): AAbsStr
  def newObject(l_proto: ALoc, cls: String, bExt: AAbsBool = BoolTrue): AAbsObj
  def newArrayObject(v: AAbsNum): AAbsObj
  val newDObject: AAbsDObject
  def sysLoc(name: String): ALoc
  def gamma_str(s: AAbsStr): Option[Set[String]]
  def filterCallable(s: AAbsState, ls: AAbsValue): AAbsValue
  def isNonEmpty(v: AAbsValue): Boolean
  def newAddr(): ALoc
  def callToString(s: SFInput)(v: AAbsValue): Set[AAbsStr]
  def oldify(l: ALoc)(s: AAbsState): AAbsState
  def doldify(l: Long)(s: AAbsState): AAbsState

  def toIValue(dl: DLoc.T): AAbsIValue
  def toIValue(l: AAbsValue): AAbsIValue

  def DtoValue(dl: DLoc.T): AAbsValue
  def LtoValue(l: ALoc): AAbsValue
  def NtoValue(n: AAbsNum): AAbsValue
  def StoValue(s: AAbsStr): AAbsValue
  def ItoValue(i: AAbsIValue): AAbsValue
  def NgetSingle(v: AAbsValue): Option[Double]

  def setInterval(handlers: AAbsValue, addr: ALoc)(s: AAbsState): (AAbsValue, AAbsState)

  val BoolTrue: AAbsBool
  val BoolFalse: AAbsBool
  val OtherStr: AAbsStr
  val IntStr: AAbsStr
  val UInt: AAbsNum
  val GlobalLoc: ALoc
  val ValueBot: AAbsValue
  val StateBot: AAbsState
  val LocSetBot: AbsLoc
}