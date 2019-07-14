import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser ircbot.unicode
open types support parser

namespace ircbot_external.urls

def Scheme : parser string :=
str "http://" >> pure "http://" <|>
str "https://" >> pure "https://"

def Url : parser string := do
(++) <$> Scheme <*> many_char1 parsing.WordChar <* optional (ch '.')

def delims : list char := [' ', '\t', ',', ';', '|']

def get_urls (text : string) : list string :=
list.filter_map
  (λ word, sum.cases_on (run_string Url word) (λ _, none) some) $
    text.split (∈ delims)

def timeout := 2
def max_length := 30 * 1024

def get_page_by_url (url : string) : io string := do
  curl_proc ← io.proc.spawn
    { cmd := "curl",
      args := [ "--max-time", to_string timeout, "--silent", "--no-keepalive", "--location", url ],
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

def get_title (page : string) :=
get_title_of_tokens (page.split (∈ ['<', '>']))

def title_notice (channel : string) (title : string) :=
notice channel $ sformat! "Title: {title}"

def titles : bot_function :=
  { name := "title",
    syntax := none,
    description := "Returns URL titles.",
    func := λ raw_input, do input ← raw_input,
      match input with
      | irc_text.parsed_normal
        { object := some object, type := message.privmsg,
          args := [subject], text := text } := 
        if subject.front = '#' then do
          pages ← sequence $ get_page_by_url <$> get_urls text,
          pure $ title_notice subject <$> list.filter_map get_title pages
        else pure []
      | _ := pure []
      end }

end ircbot_external.urls