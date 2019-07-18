import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser ircbot.unicode
open types support parser

namespace ircbot_external

-- https://www.w3.org/TR/html4/sgml/entities.html
def entities :=
  [ ("nbsp", 160), ("iexcl", 161), ("cent", 162), ("pound", 163),
    ("curren", 164), ("yen", 165), ("brvbar", 166), ("sect", 167),
    ("uml", 168), ("copy", 169), ("ordf", 170), ("laquo", 171),
    ("not", 172), ("shy", 173), ("reg", 174), ("macr", 175),
    ("deg", 176), ("plusmn", 177), ("sup2", 178), ("sup3", 179),
    ("acute", 180), ("micro", 181), ("para", 182), ("middot", 183),
    ("cedil", 184), ("sup1", 185), ("ordm", 186), ("raquo", 187),
    ("frac14", 188), ("frac12", 189), ("frac34", 190), ("iquest", 191),
    ("Agrave", 192), ("Aacute", 193), ("Acirc", 194), ("Atilde", 195),
    ("Auml", 196), ("Aring", 197), ("AElig", 198), ("Ccedil", 199),
    ("Egrave", 200), ("Eacute", 201), ("Ecirc", 202), ("Euml", 203),
    ("Igrave", 204), ("Iacute", 205), ("Icirc", 206), ("Iuml", 207),
    ("ETH", 208), ("Ntilde", 209), ("Ograve", 210), ("Oacute", 211),
    ("Ocirc", 212), ("Otilde", 213), ("Ouml", 214), ("times", 215),
    ("Oslash", 216), ("Ugrave", 217), ("Uacute", 218), ("Ucirc", 219),
    ("Uuml", 220), ("Yacute", 221), ("THORN", 222), ("szlig", 223),
    ("agrave", 224), ("aacute", 225), ("acirc", 226), ("atilde", 227),
    ("auml", 228), ("aring", 229), ("aelig", 230), ("ccedil", 231),
    ("egrave", 232), ("eacute", 233), ("ecirc", 234), ("euml", 235),
    ("igrave", 236), ("iacute", 237), ("icirc", 238), ("iuml", 239),
    ("eth", 240), ("ntilde", 241), ("ograve", 242), ("oacute", 243),
    ("ocirc", 244), ("otilde", 245), ("ouml", 246), ("divide", 247),
    ("oslash", 248), ("ugrave", 249), ("uacute", 250), ("ucirc", 251),
    ("uuml", 252), ("yacute", 253), ("thorn", 254), ("yuml", 255),
    ("fnof", 402), ("Alpha", 913), ("Beta", 914), ("Gamma", 915),
    ("Delta", 916), ("Epsilon", 917), ("Zeta", 918), ("Eta", 919),
    ("Theta", 920), ("Iota", 921), ("Kappa", 922), ("Lambda", 923),
    ("Mu", 924), ("Nu", 925), ("Xi", 926), ("Omicron", 927),
    ("Pi", 928), ("Rho", 929), ("Sigma", 931), ("Tau", 932),
    ("Upsilon", 933), ("Phi", 934), ("Chi", 935), ("Psi", 936),
    ("Omega", 937), ("alpha", 945), ("beta", 946), ("gamma", 947),
    ("delta", 948), ("epsilon", 949), ("zeta", 950), ("eta", 951),
    ("theta", 952), ("iota", 953), ("kappa", 954), ("lambda", 955),
    ("mu", 956), ("nu", 957), ("xi", 958), ("omicron", 959),
    ("pi", 960), ("rho", 961), ("sigmaf", 962), ("sigma", 963),
    ("tau", 964), ("upsilon", 965), ("phi", 966), ("chi", 967),
    ("psi", 968), ("omega", 969), ("thetasym", 977), ("upsih", 978),
    ("piv", 982), ("bull", 8226), ("hellip", 8230), ("prime", 8242),
    ("Prime", 8243), ("oline", 8254), ("frasl", 8260), ("weierp", 8472),
    ("image", 8465), ("real", 8476), ("trade", 8482), ("alefsym", 8501),
    ("larr", 8592), ("uarr", 8593), ("rarr", 8594), ("darr", 8595),
    ("harr", 8596), ("crarr", 8629), ("lArr", 8656), ("uArr", 8657),
    ("rArr", 8658), ("dArr", 8659), ("hArr", 8660), ("forall", 8704),
    ("part", 8706), ("exist", 8707), ("empty", 8709), ("nabla", 8711),
    ("isin", 8712), ("notin", 8713), ("ni", 8715), ("prod", 8719),
    ("sum", 8721), ("minus", 8722), ("lowast", 8727), ("radic", 8730),
    ("prop", 8733), ("infin", 8734), ("ang", 8736), ("and", 8743),
    ("or", 8744), ("cap", 8745), ("cup", 8746), ("int", 8747),
    ("there4", 8756), ("sim", 8764), ("cong", 8773), ("asymp", 8776),
    ("ne", 8800), ("equiv", 8801), ("le", 8804), ("ge", 8805),
    ("sub", 8834), ("sup", 8835), ("nsub", 8836), ("sube", 8838),
    ("supe", 8839), ("oplus", 8853), ("otimes", 8855), ("perp", 8869),
    ("sdot", 8901), ("lceil", 8968), ("rceil", 8969), ("lfloor", 8970),
    ("rfloor", 8971), ("lang", 9001), ("rang", 9002), ("loz", 9674),
    ("spades", 9824), ("clubs", 9827), ("hearts", 9829), ("diams", 9830),
    ("quot", 34), ("amp", 38), ("lt", 60), ("gt", 62),
    ("OElig", 338), ("oelig", 339), ("Scaron", 352), ("scaron", 353),
    ("Yuml", 376), ("circ", 710), ("tilde", 732), ("ensp", 8194),
    ("emsp", 8195), ("thinsp", 8201), ("zwnj", 8204), ("zwj", 8205),
    ("lrm", 8206), ("rlm", 8207), ("ndash", 8211), ("mdash", 8212),
    ("lsquo", 8216), ("rsquo", 8217), ("sbquo", 8218), ("ldquo", 8220),
    ("rdquo", 8221), ("bdquo", 8222), ("dagger", 8224), ("Dagger", 8225),
    ("permil", 8240), ("lsaquo", 8249), ("rsaquo", 8250), ("euro", 8364) ]

namespace urls
  structure curl_conf :=
  (retry : ℕ) (retry_max_time : ℕ) (max_time : ℕ)

  def NotAmpersand : parser char := sat (≠ '&')
  def DecOrHex : parser nat :=
  parsing.Number <|> (ch 'x' <|> ch 'X') >> parsing.HexNumber

  def NumCharRef : parser char :=
  str "&#" >> char.of_nat <$> DecOrHex <* ch ';'

  def Ent (name : string) (value : ℕ) : parser char :=
  str ("&" ++ name ++ ";") >> pure (char.of_nat value)

  def CharEntRef : parser char :=
  list.foldr (λ ent par, Ent (prod.fst ent) (prod.snd ent) <|> par)
    (parser.fail "unknown entity") entities

  def Quoted : parser string :=
  many_char (NotAmpersand <|> CharEntRef <|> NumCharRef)

  def curl_conf.args (x : curl_conf) : list string :=
  [ "--retry", to_string x.retry,
    "--retry-max-time", to_string x.retry_max_time,
    "--max-time", to_string x.max_time,
    "--silent", "--no-keepalive" ]

  def Prefix : parser string :=
  many_char $ sat (not ∘ char.is_alpha)

  def Scheme : parser string :=
  str "http://" >> pure "http://" <|>
  str "https://" >> pure "https://"

  def Url : parser string := do
  (++) <$> (Prefix >> Scheme) <*> many_char1 parsing.WordChar <* optional (ch '.')

  def delims : list char := [' ', '\t', ',', ';', '|']

  def get_urls (text : string) : list string :=
  list.filter_map
    (λ word, sum.cases_on (run_string Url word) (λ _, none) some) $
      text.split (∈ delims)

  def conf : curl_conf :=
  { retry := 5,
    retry_max_time := 5,
    max_time := 1 }

  def max_length := 150 * 1024

  def get_page_by_url (url : string) : io string := do
    curl_proc ← io.proc.spawn
      { cmd := "curl",
        args := conf.args ++ [ "--location", url ],
        stdout := io.process.stdio.piped },
    page ← io.fs.read curl_proc.stdout max_length,
    io.fs.close curl_proc.stdout,
    exitv ← io.proc.wait curl_proc,
    when (exitv ≠ 0) $ io.put_str_ln $ sformat! "! process exited with status {exitv}",
    pure page.to_string

  def get_title_of_tokens : list string → option string
  | (start :: content :: close :: tl) :=
    if start = "title" ∧ close = "/title" then some content
    else get_title_of_tokens (content :: close :: tl)
  | (hd :: tl) := get_title_of_tokens tl
  | [] := none

  def tokenize : string → list string :=
  string.split (∈ ['<', '>'])

  def get_title (page : string) : option string :=
  match run_string Quoted <$> get_title_of_tokens (tokenize page) with
  | some (sum.inr title) := some title
  | _ := none
  end

  def title_notice (channel : string) (title : string) :=
  notice channel $ sformat! "Title: {title}"
end urls

def urls : bot_function :=
  { name := "title",
    syntax := none,
    description := "Returns URL titles.",
    func := λ input,
      match input with
      | irc_text.parsed_normal
        { object := some object, type := message.privmsg,
          args := [subject], text := text } := 
        if subject.front = '#' then do
          pages ← sequence $ urls.get_page_by_url <$> urls.get_urls text,
          pure $ urls.title_notice subject <$> list.filter_map urls.get_title pages
        else pure []
      | _ := pure []
      end }

end ircbot_external