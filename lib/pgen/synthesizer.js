function init() {
  let lexicalElem = document.getElementById('sec-lexical-grammar');
  let exprElem = document.getElementById('sec-expressions');
  let stmtElem = document.getElementById('sec-statements');
  let funcElem = document.getElementById('sec-functions-and-classes');
  let topElem = document.getElementById('sec-scripts-and-modules');

  // Lexer.scala
  let lex = getLexical(lexicalElem);
  saveDump(lex, "Lexer.scala");

  // Node.scala
  let node = '// Abstract Syntax Tree';
  node += getLexicalNode(lexicalElem, 'lexer');
  node += getNode(exprElem, 'expressions');
  node += getNode(stmtElem, 'statements');
  node += getNode(funcElem, 'functions and classes');
  node += getNode(topElem, 'scripts and modules');
  saveDump(node, "Node.scala");
}
init();

////////////////////////////////////////////////////////////////////////////////
// DOM Element -> Rule
////////////////////////////////////////////////////////////////////////////////
function isText(node) {
  return Text.prototype.isPrototypeOf(node);
}

function getGprose(gprose) {
  let text = gprose.innerText.trim();
  if (text == 'any Unicode code point') return 'unicode';
  if (text == 'any Unicode code point with the Unicode property “ID_Start”') return 'ID_Start';
  if (text == 'any Unicode code point with the Unicode property “ID_Continue”') return 'ID_Continue';
  else return text.substr(1, text.length - 2);
}

function getT(t) {
  return t.innerText.trim();
}

function getGann(gann) {
  if (gann.childNodes[0].textContent.indexOf('empty') != -1) {
    return { kind: 'empty' }
  }
  let arr = [[]];
  let cur = arr[0];
  for (child of gann.childNodes) {
    if (isText(child)) {
      if (child.textContent.trim() == ',') {
        cur = [];
        arr.push(cur);
      }
    } else {
      cur.push(getToken(child));
    }
  }
  return {
    kind: 'lookahead',
    data: arr
  };
}

function getParam(str) {
  str = str.trim();
  switch (str[0]) {
    case '+': return 'true';
    case '~': return 'false';
    case '?': return str.substr(1);
    default: return str;
  }
}

function getConstraint(str) {
  str = str.trim();
  let name = str.substr(1);
  switch (str[0]) {
    case '+': return name;
    case '~': return '!' + name;
  }
}

function getNt(elem) {
  let nt = {};

  // name
  nt.name = elem.getElementsByTagName('a')[0].innerText.trim();

  // params
  let params = elem.getAttribute('params');
  if (params != null) nt.params = params.split(',').map(x => getParam(x));
  else nt.params = [];

  // optional
  let optional = elem.getAttribute('optional');
  if (optional != null) nt.optional = true
  else nt.optional = false;

  return nt;
}

function getLhs(prod) {
  return getNt(prod.getElementsByTagName('emu-nt')[0]);
}

function getToken(elem) {
  let tag = elem.tagName.substr(4).toLowerCase();
  let gen;
  switch (tag) {
    case 'nt': gen = getNt; break;
    case 't': gen = getT; break;
    case 'gprose': gen = getGprose; break;
    case 'gann': gen = getGann; break;
    case 'constraints': return undefined;
    case 'gmod': return undefined;
    default: console.error('Unhandeled tag: ' + tag); return undefined;
  }
  return {
    tag: tag,
    inner: gen(elem)
  };
}

function getRhs(elem) {
  let rhs = {};

  // constraint
  let constraint = elem.getAttribute('constraints');
  if (constraint != null) rhs.constraint = getConstraint(constraint);

  // tokens
  rhs.tokenList = [];

  for (child of elem.children) {
    let token = getToken(child);
    if (token !== undefined) {
      if (token.tag == 'gmod') rhs.gmod = token.inner;
      else rhs.tokenList.push(token);
    }
  }
  return rhs;
}

function getRhsList(prod) {
  let rhsList = [];
  for (rhs of prod.getElementsByTagName('emu-rhs')) {
    rhsList.push(getRhs(rhs))
  }
  return rhsList;
}

function isOneOf(prod) {
  return prod.getAttribute('oneof') != null;
}

function getProd(elem) {
  let prod = {};
  prod.lhs = getLhs(elem);
  prod.rhsList = getRhsList(elem);
  if (isOneOf(elem)) prod.oneOf = true;
  else prod.oneOf = false;

  return prod;
}

////////////////////////////////////////////////////////////////////////////////
// Rule -> Lexer
////////////////////////////////////////////////////////////////////////////////
// get lexical grammar
function getLexical(lexical) {
  let code = `import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
  def opt(parser: Parser[String]): Parser[String] = parser | success("")
  def seq(parsers: Parser[String]*): Parser[String] = seq(parsers.toList)
  def seq(parsers: List[Parser[String]]): Parser[String] = parsers match {
    case Nil => success("")
    case hd :: tl => hd ~ seq(tl) ^^ { case hd ~ tl => hd + tl }
  }
  def noLookAhead(parser: Parser[String]): Parser[String] = not(parser) ^^^ ""

  val ZWNJ = "\\u200C"
  val ZWJ = "\\u200D"
  val ZWNBSP = "\\uFEFF"

  val LF = "\\u000A"
  val CR = "\\u000D"
  val LS = "\\u2028"
  val PS = "\\u2029"

  val TAB = "\\u0009"
  val VT = "\\u000B"
  val FF = "\\u000C"
  val SP = "\\u0020"
  val NBSP = "\\u00A0"
  val USP = "[\\u1680\\u2000\\u2001\\u2002\\u2003\\u2004\\u2005\\u2006\\u2007\\u2008\\u2009\\u200A\\u202F\\u205F\\u3000]".r

  val ID_Start = """[A-Za-zªµºÀ-ÖØ-öø-ˁˆ-ˑ ˠ-ˤˬˮͰ-ʹͶͷͺ-ͽͿΆΈ-ΊΌΎ-Ρ Σ-ϵϷ-ҁҊ-ԯԱ-Ֆՙՠ-ֈא-תׯ-ײ ؠ-يٮٯٱ-ۓەۥۦۮۯۺ-ۼۿܐܒ-ܯ ݍ-ޥޱߊ-ߪߴߵߺࠀ-ࠕࠚࠤࠨࡀ-ࡘࡠ-ࡪ ࢠ-ࢴࢶ-ࢽऄ-हऽॐक़-ॡॱ-ঀঅ-ঌএ ঐও-নপ-রলশ-হঽৎড়ঢ়য়-ৡৰৱৼ ਅ-ਊਏਐਓ-ਨਪ-ਰਲਲ਼ਵਸ਼ਸਹਖ਼-ੜਫ਼ ੲ-ੴઅ-ઍએ-ઑઓ-નપ-રલળવ-હઽ ૐૠૡૹଅ-ଌଏଐଓ-ନପ-ରଲଳଵ-ହଽ ଡ଼ଢ଼ୟ-ୡୱஃஅ-ஊஎ-ஐஒ-கஙசஜஞட ணதந-பம-ஹௐఅ-ఌఎ-ఐఒ-నప-హ ఽౘ-ౚౠౡಀಅ-ಌಎ-ಐಒ-ನಪ-ಳವ-ಹ ಽೞೠೡೱೲഅ-ഌഎ-ഐഒ-ഺഽൎൔ-ൖൟ-ൡ ൺ-ൿඅ-ඖක-නඳ-රලව-ෆก-ะาำ เ-ๆກຂຄຆ-ຊຌ-ຣລວ-ະາຳຽເ-ໄ ໆໜ-ໟༀཀ-ཇཉ-ཬྈ-ྌက-ဪဿၐ-ၕ ၚ-ၝၡၥၦၮ-ၰၵ-ႁႎႠ-ჅჇჍა-ჺ ჼ-ቈቊ-ቍቐ-ቖቘቚ-ቝበ-ኈኊ-ኍነ-ኰ ኲ-ኵኸ-ኾዀዂ-ዅወ-ዖዘ-ጐጒ-ጕጘ-ፚ ᎀ-ᎏᎠ-Ᏽᏸ-ᏽᐁ-ᙬᙯ-ᙿᚁ-ᚚᚠ-ᛪ ᛮ-ᛸᜀ-ᜌᜎ-ᜑᜠ-ᜱᝀ-ᝑᝠ-ᝬᝮ-ᝰ ក-ឳៗៜᠠ-ᡸᢀ-ᢨᢪᢰ-ᣵᤀ-ᤞᥐ-ᥭ ᥰ-ᥴᦀ-ᦫᦰ-ᧉᨀ-ᨖᨠ-ᩔᪧᬅ-ᬳᭅ-ᭋ ᮃ-ᮠᮮᮯᮺ-ᯥᰀ-ᰣᱍ-ᱏᱚ-ᱽᲀ-ᲈᲐ-Ჺ Ჽ-Ჿᳩ-ᳬᳮ-ᳳᳵᳶᳺᴀ-ᶿḀ-ἕἘ-Ἕ ἠ-ὅὈ-Ὅὐ-ὗὙὛὝὟ-ώᾀ-ᾴᾶ-ᾼ ιῂ-ῄῆ-ῌῐ-ΐῖ-Ίῠ-Ῥῲ-ῴῶ-ῼ ⁱⁿₐ-ₜℂℇℊ-ℓℕ℘-ℝℤΩℨK-ℹℼ-ℿ ⅅ-ⅉⅎⅠ-ↈⰀ-Ⱞⰰ-ⱞⱠ-ⳤⳫ-ⳮⳲⳳ ⴀ-ⴥⴧⴭⴰ-ⵧⵯⶀ-ⶖⶠ-ⶦⶨ-ⶮⶰ-ⶶ ⶸ-ⶾⷀ-ⷆⷈ-ⷎⷐ-ⷖⷘ-ⷞ々-〇〡-〩 〱-〵〸-〼ぁ-ゖ゛-ゟァ-ヺー-ヿㄅ-ㄯ ㄱ-ㆎㆠ-ㆺㇰ-ㇿ㐀-䶵一-鿯ꀀ-ꒌꓐ-ꓽ ꔀ-ꘌꘐ-ꘟꘪꘫꙀ-ꙮꙿ-ꚝꚠ-ꛯꜗ-ꜟꜢ-ꞈ Ꞌ-ꞿꟂ-Ᶎꟷ-ꠁꠃ-ꠅꠇ-ꠊꠌ-ꠢꡀ-ꡳ ꢂ-ꢳꣲ-ꣷꣻꣽꣾꤊ-ꤥꤰ-ꥆꥠ-ꥼꦄ-ꦲ ꧏꧠ-ꧤꧦ-ꧯꧺ-ꧾꨀ-ꨨꩀ-ꩂꩄ-ꩋꩠ-ꩶ ꩺꩾ-ꪯꪱꪵꪶꪹ-ꪽꫀꫂꫛ-ꫝꫠ-ꫪꫲ-ꫴ ꬁ-ꬆꬉ-ꬎꬑ-ꬖꬠ-ꬦꬨ-ꬮꬰ-ꭚꭜ-ꭧ ꭰ-ꯢ가-힣ힰ-ퟆퟋ-ퟻ豈-舘並-龎ﬀ-ﬆ ﬓ-ﬗיִײַ-ﬨשׁ-זּטּ-לּמּנּסּףּפּצּ-ﮱ ﯓ-ﴽﵐ-ﶏﶒ-ﷇﷰ-ﷻﹰ-ﹴﹶ-ﻼＡ-Ｚ ａ-ｚｦ-ﾾￂ-ￇￊ-ￏￒ-ￗￚ-ￜ𐀀-𐀋 𐀍-𐀦𐀨-𐀺𐀼𐀽𐀿-𐁍𐁐-𐁝𐂀-𐃺𐅀-𐅴𐊀-𐊜 𐊠-𐋐𐌀-𐌟𐌭-𐍊𐍐-𐍵𐎀-𐎝𐎠-𐏃𐏈-𐏏 𐏑-𐏕𐐀-𐒝𐒰-𐓓𐓘-𐓻𐔀-𐔧𐔰-𐕣𐘀-𐜶 𐝀-𐝕𐝠-𐝧𐠀-𐠅𐠈𐠊-𐠵𐠷𐠸𐠼𐠿-𐡕𐡠-𐡶 𐢀-𐢞𐣠-𐣲𐣴𐣵𐤀-𐤕𐤠-𐤹𐦀-𐦷𐦾𐦿𐨀𐨐-𐨓 𐨕-𐨗𐨙-𐨵𐩠-𐩼𐪀-𐪜𐫀-𐫇𐫉-𐫤𐬀-𐬵 𐭀-𐭕𐭠-𐭲𐮀-𐮑𐰀-𐱈𐲀-𐲲𐳀-𐳲𐴀-𐴣 𐼀-𐼜𐼧𐼰-𐽅𐿠-𐿶𑀃-𑀷𑂃-𑂯𑃐-𑃨𑄃-𑄦 𑅄𑅐-𑅲𑅶𑆃-𑆲𑇁-𑇄𑇚𑇜𑈀-𑈑𑈓-𑈫𑊀-𑊆 𑊈𑊊-𑊍𑊏-𑊝𑊟-𑊨𑊰-𑋞𑌅-𑌌𑌏𑌐𑌓-𑌨 𑌪-𑌰𑌲𑌳𑌵-𑌹𑌽𑍐𑍝-𑍡𑐀-𑐴𑑇-𑑊𑑟𑒀-𑒯 𑓄𑓅𑓇𑖀-𑖮𑗘-𑗛𑘀-𑘯𑙄𑚀-𑚪𑚸𑜀-𑜚𑠀-𑠫 𑢠-𑣟𑣿𑦠-𑦧𑦪-𑧐𑧡𑧣𑨀𑨋-𑨲𑨺𑩐𑩜-𑪉 𑪝𑫀-𑫸𑰀-𑰈𑰊-𑰮𑱀𑱲-𑲏𑴀-𑴆𑴈𑴉𑴋-𑴰 𑵆𑵠-𑵥𑵧𑵨𑵪-𑶉𑶘𑻠-𑻲𒀀-𒎙𒐀-𒑮𒒀-𒕃 𓀀-𓐮𔐀-𔙆𖠀-𖨸𖩀-𖩞𖫐-𖫭𖬀-𖬯𖭀-𖭃 𖭣-𖭷𖭽-𖮏𖹀-𖹿𖼀-𖽊𖽐𖾓-𖾟𖿠𖿡𖿣𗀀-𘟷 𘠀-𘫲𛀀-𛄞𛅐-𛅒𛅤-𛅧𛅰-𛋻𛰀-𛱪𛱰-𛱼 𛲀-𛲈𛲐-𛲙𝐀-𝑔𝑖-𝒜𝒞𝒟𝒢𝒥𝒦𝒩-𝒬𝒮-𝒹 𝒻𝒽-𝓃𝓅-𝔅𝔇-𝔊𝔍-𝔔𝔖-𝔜𝔞-𝔹𝔻-𝔾 𝕀-𝕄𝕆𝕊-𝕐𝕒-𝚥𝚨-𝛀𝛂-𝛚𝛜-𝛺𝛼-𝜔 𝜖-𝜴𝜶-𝝎𝝐-𝝮𝝰-𝞈𝞊-𝞨𝞪-𝟂𝟄-𝟋 𞄀-𞄬𞄷-𞄽𞅎𞋀-𞋫𞠀-𞣄𞤀-𞥃𞥋𞸀-𞸃𞸅-𞸟 𞸡𞸢𞸤𞸧𞸩-𞸲𞸴-𞸷𞸹𞸻𞹂𞹇𞹉𞹋𞹍-𞹏𞹑𞹒 𞹔𞹗𞹙𞹛𞹝𞹟𞹡𞹢𞹤𞹧-𞹪𞹬-𞹲𞹴-𞹷𞹹-𞹼 𞹾𞺀-𞺉𞺋-𞺛𞺡-𞺣𞺥-𞺩𞺫-𞺻𠀀-𪛖𪜀-𫜴 𫝀-𫠝𫠠-𬺡𬺰-𮯠丽-𪘀]""".r

  val ID_Continue = """[0-9A-Z_a-zªµ·ºÀ-ÖØ-ö ø-ˁˆ-ˑˠ-ˤˬˮ̀-ʹͶͷͺ-ͽͿΆ-Ί ΌΎ-ΡΣ-ϵϷ-ҁ҃-҇Ҋ-ԯԱ-Ֆՙՠ-ֈ֑-ׇֽֿׁׂׅׄ א-תׯ-ײؐ-ؚؠ-٩ٮ-ۓە-ۜ۟-۪ۨ-ۼ ۿܐ-݊ݍ-ޱ߀-ߵߺ߽ࠀ-࠭ࡀ-࡛ࡠ-ࡪ ࢠ-ࢴࢶ-ࢽ࣓-ࣣ࣡-ॣ०-९ॱ-ঃঅ-ঌ এঐও-নপ-রলশ-হ়-ৄেৈো-ৎৗ ড়ঢ়য়-ৣ০-ৱৼ৾ਁ-ਃਅ-ਊਏਐਓ-ਨ ਪ-ਰਲਲ਼ਵਸ਼ਸਹ਼ਾ-ੂੇੈੋ-੍ੑਖ਼-ੜ ਫ਼੦-ੵઁ-ઃઅ-ઍએ-ઑઓ-નપ-રલળ વ-હ઼-ૅે-ૉો-્ૐૠ-ૣ૦-૯ૹ-૿ଁ-ଃ ଅ-ଌଏଐଓ-ନପ-ରଲଳଵ-ହ଼-ୄେୈୋ-୍ୖୗ ଡ଼ଢ଼ୟ-ୣ୦-୯ୱஂஃஅ-ஊஎ-ஐஒ-கங சஜஞடணதந-பம-ஹா-ூெ-ைொ-் ௐௗ௦-௯ఀ-ఌఎ-ఐఒ-నప-హఽ-ౄె-ైొ-్ౕౖ ౘ-ౚౠ-ౣ౦-౯ಀ-ಃಅ-ಌಎ-ಐಒ-ನ ಪ-ಳವ-ಹ಼-ೄೆ-ೈೊ-್ೕೖೞೠ-ೣ ೦-೯ೱೲഀ-ഃഅ-ഌഎ-ഐഒ-ൄെ-ൈൊ-ൎ ൔ-ൗൟ-ൣ൦-൯ൺ-ൿංඃඅ-ඖක-නඳ-ර ලව-ෆ්ා-ුූෘ-ෟ෦-෯ෲෳก-ฺเ-๎ ๐-๙ກຂຄຆ-ຊຌ-ຣລວ-ຽເ-ໄໆ່-ໍ ໐-໙ໜ-ໟༀ༘༙༠-༩༹༵༷༾-ཇཉ-ཬཱ-྄྆-ྗྙ-ྼ࿆ က-၉ၐ-ႝႠ-ჅჇჍა-ჺჼ-ቈቊ-ቍቐ-ቖ ቘቚ-ቝበ-ኈኊ-ኍነ-ኰኲ-ኵኸ-ኾዀዂ-ዅ ወ-ዖዘ-ጐጒ-ጕጘ-ፚ፝-፟፩-፱ᎀ-ᎏ Ꭰ-Ᏽᏸ-ᏽᐁ-ᙬᙯ-ᙿᚁ-ᚚᚠ-ᛪᛮ-ᛸ ᜀ-ᜌᜎ-᜔ᜠ-᜴ᝀ-ᝓᝠ-ᝬᝮ-ᝰᝲᝳក-៓ ៗៜ៝០-៩-᠐-᠙ᠠ-ᡸᢀ-ᢪᢰ-ᣵᤀ-ᤞᤠ-ᤫᤰ-᤻ ᥆-ᥭᥰ-ᥴᦀ-ᦫᦰ-ᧉ᧐-᧚ᨀ-ᨛᨠ-ᩞ᩠-᩿᩼-᪉ ᪐-᪙ᪧ᪰-᪽ᬀ-ᭋ᭐-᭙᭫-᭳ᮀ-᯳ᰀ-᰷ ᱀-᱉ᱍ-ᱽᲀ-ᲈᲐ-ᲺᲽ-Ჿ᳐-᳔᳒-ᳺ ᴀ-᷹᷻-ἕἘ-Ἕἠ-ὅὈ-Ὅὐ-ὗὙὛὝ Ὗ-ώᾀ-ᾴᾶ-ᾼιῂ-ῄῆ-ῌῐ-ΐῖ-Ί ῠ-Ῥῲ-ῴῶ-ῼ‿⁀⁔ⁱⁿₐ-ₜ⃐-⃥⃜⃡-⃰ ℂℇℊ-ℓℕ℘-ℝℤΩℨK-ℹℼ-ℿⅅ-ⅉ ⅎⅠ-ↈⰀ-Ⱞⰰ-ⱞⱠ-ⳤⳫ-ⳳⴀ-ⴥⴧⴭ ⴰ-ⵧⵯ⵿-ⶖⶠ-ⶦⶨ-ⶮⶰ-ⶶⶸ-ⶾⷀ-ⷆ ⷈ-ⷎⷐ-ⷖⷘ-ⷞⷠ-ⷿ々-〇〡-〯〱-〵 〸-〼ぁ-ゖ゙-ゟァ-ヺー-ヿㄅ-ㄯㄱ-ㆎ ㆠ-ㆺㇰ-ㇿ㐀-䶵一-鿯ꀀ-ꒌꓐ-ꓽꔀ-ꘌ ꘐ-ꘫꙀ-꙯ꙴ-꙽ꙿ-꛱ꜗ-ꜟꜢ-ꞈꞋ-ꞿ Ꟃ-Ᶎꟷ-ꠧꡀ-ꡳꢀ-ꣅ꣐-꣙꣠-ꣷꣻꣽ-꤭ ꤰ-꥓ꥠ-ꥼꦀ-꧀ꧏ-꧙ꧠ-ꧾꨀ-ꨶꩀ-ꩍ ꩐-꩙ꩠ-ꩶꩺ-ꫂꫛ-ꫝꫠ-ꫯꫲ-꫶ꬁ-ꬆ ꬉ-ꬎꬑ-ꬖꬠ-ꬦꬨ-ꬮꬰ-ꭚꭜ-ꭧꭰ-ꯪ꯬꯭ ꯰-꯹가-힣ힰ-ퟆퟋ-ퟻ豈-舘並-龎ﬀ-ﬆ ﬓ-ﬗיִ-ﬨשׁ-זּטּ-לּמּנּסּףּפּצּ-ﮱﯓ-ﴽ ﵐ-ﶏﶒ-ﷇﷰ-ﷻ-︠-︯︳︴﹍-﹏ﹰ-ﹴ ﹶ-ﻼ０-９Ａ-Ｚ＿ａ-ｚｦ-ﾾￂ-ￇￊ-ￏ ￒ-ￗￚ-ￜ𐀀-𐀋𐀍-𐀦𐀨-𐀺𐀼𐀽𐀿-𐁍𐁐-𐁝 𐂀-𐃺𐅀-𐅴𐇽𐊀-𐊜𐊠-𐋐𐋠𐌀-𐌟𐌭-𐍊𐍐-𐍺 𐎀-𐎝𐎠-𐏃𐏈-𐏏𐏑-𐏕𐐀-𐒝𐒠-𐒩𐒰-𐓓 𐓘-𐓻𐔀-𐔧𐔰-𐕣𐘀-𐜶𐝀-𐝕𐝠-𐝧𐠀-𐠅 𐠈𐠊-𐠵𐠷𐠸𐠼𐠿-𐡕𐡠-𐡶𐢀-𐢞𐣠-𐣲𐣴𐣵 𐤀-𐤕𐤠-𐤹𐦀-𐦷𐦾𐦿𐨀-𐨃𐨅𐨆𐨌-𐨓𐨕-𐨗 𐨙-𐨵𐨸-𐨿𐨺𐩠-𐩼𐪀-𐪜𐫀-𐫇𐫉-𐫦𐬀-𐬵 𐭀-𐭕𐭠-𐭲𐮀-𐮑𐰀-𐱈𐲀-𐲲𐳀-𐳲𐴀-𐴧 𐴰-𐴹𐼀-𐼜𐼧𐼰-𐽐𐿠-𐿶𑀀-𑁆𑁦-𑁯𑁿-𑂺 𑃐-𑃨𑃰-𑃹𑄀-𑄴𑄶-𑄿𑅄-𑅆𑅐-𑅳𑅶𑆀-𑇄𑇉-𑇌 𑇐-𑇚𑇜𑈀-𑈑𑈓-𑈷𑈾𑊀-𑊆𑊈𑊊-𑊍𑊏-𑊝 𑊟-𑊨𑊰-𑋪𑋰-𑋹𑌀-𑌃𑌅-𑌌𑌏𑌐𑌓-𑌨𑌪-𑌰 𑌲𑌳𑌵-𑌹𑌻-𑍄𑍇𑍈𑍋-𑍍𑍐𑍗𑍝-𑍣𑍦-𑍬𑍰-𑍴 𑐀-𑑊𑑐-𑑙𑑞𑑟𑒀-𑓅𑓇𑓐-𑓙𑖀-𑖵𑖸-𑗀 𑗘-𑗝𑘀-𑙀𑙄𑙐-𑙙𑚀-𑚸𑛀-𑛉𑜀-𑜚𑜝-𑜫 𑜰-𑜹𑠀-𑠺𑢠-𑣩𑣿𑦠-𑦧𑦪-𑧗𑧚-𑧡𑧣𑧤 𑨀-𑨾𑩇𑩐-𑪙𑪝𑫀-𑫸𑰀-𑰈𑰊-𑰶𑰸-𑱀𑱐-𑱙 𑱲-𑲏𑲒-𑲧𑲩-𑲶𑴀-𑴆𑴈𑴉𑴋-𑴶𑴺𑴼𑴽𑴿-𑵇 𑵐-𑵙𑵠-𑵥𑵧𑵨𑵪-𑶎𑶐𑶑𑶓-𑶘𑶠-𑶩𑻠-𑻶 𒀀-𒎙𒐀-𒑮𒒀-𒕃𓀀-𓐮𔐀-𔙆𖠀-𖨸𖩀-𖩞 𖩠-𖩩𖫐-𖫭𖫰-𖫴𖬀-𖬶𖭀-𖭃𖭐-𖭙𖭣-𖭷 𖭽-𖮏𖹀-𖹿𖼀-𖽊𖽏-𖾇𖾏-𖾟𖿠𖿡𖿣𗀀-𘟷 𘠀-𘫲𛀀-𛄞𛅐-𛅒𛅤-𛅧𛅰-𛋻𛰀-𛱪𛱰-𛱼 𛲀-𛲈𛲐-𛲙𛲝𛲞𝅥-𝅩𝅭-𝅲𝅻-𝆂𝆅-𝆋𝆪-𝆭𝉂-𝉄 𝐀-𝑔𝑖-𝒜𝒞𝒟𝒢𝒥𝒦𝒩-𝒬𝒮-𝒹𝒻𝒽-𝓃 𝓅-𝔅𝔇-𝔊𝔍-𝔔𝔖-𝔜𝔞-𝔹𝔻-𝔾𝕀-𝕄 𝕆𝕊-𝕐𝕒-𝚥𝚨-𝛀𝛂-𝛚𝛜-𝛺𝛼-𝜔𝜖-𝜴 𝜶-𝝎𝝐-𝝮𝝰-𝞈𝞊-𝞨𝞪-𝟂𝟄-𝟋𝟎-𝟿𝨀-𝨶𝨻-𝩬𝩵𝪄𝪛-𝪟𝪡-𝪯𞀀-𞀆𞀈-𞀘𞀛-𞀡𞀣𞀤𞀦-𞀪 𞄀-𞄬𞄰-𞄽𞅀-𞅉𞅎𞋀-𞋹𞠀-𞣄𞣐-𞣖𞤀-𞥋 𞥐-𞥙𞸀-𞸃𞸅-𞸟𞸡𞸢𞸤𞸧𞸩-𞸲𞸴-𞸷𞸹𞸻 𞹂𞹇𞹉𞹋𞹍-𞹏𞹑𞹒𞹔𞹗𞹙𞹛𞹝𞹟𞹡𞹢𞹤𞹧-𞹪 𞹬-𞹲𞹴-𞹷𞹹-𞹼𞹾𞺀-𞺉𞺋-𞺛𞺡-𞺣𞺥-𞺩 𞺫-𞺻𠀀-𪛖𪜀-𫜴𫝀-𫠝𫠠-𬺡𬺰-𮯠丽-𪘀-]""".r

  val unicode: Parser[String] = ".".r`;
  for (elem of lexical.getElementsByTagName('emu-production')) {
    let prod = getProd(elem)
    code += `
  ${prodToStrParser(prod)}`;
  }
  code += "\n  " + prodToStrParser(getProd(document.getElementById('prod-NotEscapeSequence')));
  code += "\n  " + prodToStrParser(getProd(document.getElementById('prod-NotCodePoint')));
  code += "\n  " + prodToStrParser(getProd(document.getElementById('prod-CodePoint')));
  code += `
}`;
  return code;
}

function prodToStrParser(prod) {
  return `lazy val ${prod.lhs.name}: Parser[String] = ${rhsListToStrParser(prod.rhsList, prod.oneOf)}`;
}
function rhsListToStrParser(rhsList, oneOf) {
  return rhsList.map(rhs => rhsToStrParser(rhs, oneOf)).join(' | ');
}
function rhsToStrParser(rhs, oneOf) {
  if (rhs.gmod) {
    let parser = tokenToStrParser(rhs.tokenList[0]);
    let gmodParser = tokenListToStrParser(rhs.gmod);
    return `${parser}.filter(x => !parse(${gmodParser}, x).successful)`;
  } else return tokenListToStrParser(rhs.tokenList, !oneOf)
}
function tokenListToStrParser(tokenList, seq) {
  if (tokenList.length == 1) return tokenToStrParser(tokenList[0]);
  if (seq) return `seq(${tokenList.map(token => tokenToStrParser(token)).join(', ')})`;
  return `(${tokenList.map(token => tokenToStrParser(token)).join(' | ')})`;
}
function tokenToStrParser(token) {
  let tag = token.tag;
  let inner = token.inner;
  let code = '';
  switch (tag) {
    case 'nt':
      if (inner.optional) return `opt(${inner.name})`;
      else return inner.name;
    case 't':
      return `"${norm(inner)}"`;
    case 'gprose':
      return inner;
    case 'gann':
      if (inner.kind == 'lookahead') {
        let rules = inner.data.map(tokenList => `noLookAhead(${tokenListToStrParser(tokenList, false)})`);
        if (rules.length != 1) return `seq(${rules.join(', ')})`;
        else return rules[0];
      } else return 'success("")';
  }
}
function norm(str) {
  return str.replace(/\\/g, "\\\\").replace(/"/g, "\\\"");
}

////////////////////////////////////////////////////////////////////////////////
// Rule -> AST
////////////////////////////////////////////////////////////////////////////////
// get nodes for lexical grammars
function getLexicalNode(elems, name) {
  let code = '\n\n// ' + name;
  for (elem of elems.getElementsByTagName('emu-production')) {
    code += `
case class ${getProd(elem).lhs.name}(str: String)`;
  }
  return code;
}
// get nodes for grammars
function getNode(elems, name) {
  let code = '\n\n// ' + name;
  for (elem of elems.getElementsByTagName('emu-production')) {
    if (!isValidProdElem(elem)) continue;
    code += '\n\n' + prodToNode(getProd(elem));
  }
  return code;
}

function prodToNode(prod) {
  let name = prod.lhs.name;
  let rhsList = prod.rhsList
  let code = `trait ${name}`;
  for (let i = 0; i < rhsList.length; i++) {
    code += rhsToNode(name, i, rhsList[i]);
  }
  return code;
}

function rhsToNode(name, idx, rhs) {
  let code = `
case class ${name}${idx}(`;
  let params = [];
  let tokenList = rhs.tokenList;
  for (let i = 0; i < tokenList.length; i++) {
    let token = tokenList[i];
    if (token.tag == 'nt') params.push(tokenToNode(token.inner.name, i));
  }
  code += params.join(', ');
  code += `) extends ${name}`;
  return code;
}

function tokenToNode(name, idx) {
  return `x${idx}: ${name}`;
}

// function prodToScala(indent, prod, isStruct) {
//   let code = '';
//   let lhs = prod.lhs;
//   return lhsToScala(indent, prod.lhs, isStruct) +
//     rhsListToScala(indent, prod.rhsList, isStruct);
// }
// 
// function lhsToScala(indent, lhs, isStruct) {
//   let code = '';
//   let name = lhs.name;
//   let Params = lhs.params;
// 
//   code += indent;
//   if (params.length > 0) {
//     code += `def ${name}(${params[0]}: Boolean`;
//     for (param of params.slice(1)) {
//       code += `, ${param}: Boolean`;
//     }
//     code += ')';
//   } else {
//     code += `lazy val ${name}`;
//   }
//   code += ': Parser[';
//   if (isStruct) code += name;
//   else code += 'String';
//   code += '] =';
//   return code;
// }
// 
// function rhsListToScala(indent, rhsList, isStruct) {
//   let code = '';
//   let len = rhsList.length;
//   if (len > 0) {
//     code += '\n'
//     rhsToScala(indent + TAB, rhsList[0], isStruct);
//     for (rhs of rhsList.slice(1)) {
//       code += ' |\n'
//       rhsToScala(indent + TAB, rhs, isStruct);
//     }
//   }
//   return code;
// }
// 
// function rhs(indent, rhs, isStruct) {
//   let code = '';
//   let constraint = rhs.constraint;
//   let tokenList = rhs.tokenList;
// 
//   code += indent;
//   if (constraint) {
//     code += `(if (${constraint}) `;
//   }
//   code += elemToScala(
//   if (constraint) {
//     code += ' else failure(""))';
//   }
// }
//

////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////
function saveDump(data, filename) {
  if (data === undefined) {
    console.error('No data');
    return;
  }

  // save as jsmodel form
  if (filename === undefined) filename = 'dump.jsmodel';

  var blob = new Blob([data], {type: 'text/json'});
  e = document.createEvent('MouseEvents');
  a = document.createElement('a');

  a.download = filename;
  a.href = window.URL.createObjectURL(blob);
  a.dataset.downloadurl = ['text/json', a.download, a.href].join(':');
  e.initMouseEvent('click', true, false, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
  a.dispatchEvent(e);
}

function isValidProdElem(elem) {
  elem.parentElement.tagName == 'EMU-ANNEX';
}
