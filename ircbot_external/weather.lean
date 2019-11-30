import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser ircbot.unicode
open types support parser

namespace ircbot_external

namespace weather
  def max_length := 1024

  def format := "%l:+%C+%t+%w+%P+%p"

  def get_url (loc : string) :=
  sformat! "wttr.in/{loc}?format={format}"

  def max_time := 7

  def get_weather_by_location (loc : string) : io string := do
    curl_proc ← io.proc.spawn
      { cmd := "curl",
        args :=
          [ "--max-time", to_string max_time,
            "--silent", "--no-keepalive",
            get_url loc ],
        stdout := io.process.stdio.piped },
    page ← io.fs.read curl_proc.stdout max_length,
    io.fs.close curl_proc.stdout,
    exitv ← io.proc.wait curl_proc,
    pure $ if (exitv ≠ 0) then sformat! "curl exited with status {exitv}, sorry"
           else page.to_string

end weather

def Words : parser string :=
many_char1 (sat $ function.const char true)

structure speech :=
(object : person) (subject text : string) (type : message)

def router {α : Type} (name desc : string) (syntax : option string)
  (p : parser α) (func : speech → α → io (list irc_text)) : bot_function :=
let p' := parsing.tok ("\\" ++ name) >> p in
{ name := name,
  syntax := syntax,
  description := desc,
  func := λ input,
    match input with
    | irc_text.parsed_normal
      { object := some object, type := type,
        args := [ subject ], text := text } := 
      if subject.front ≠ '#' then
        sum.rec_on (run_string p' text) (λ _, pure [])
          (func ⟨object, subject, text, type⟩)
      else pure []
    | _ := pure []
    end }

def list.singleton {α : Type} (x : α) : list α := [ x ]

def weather : bot_function :=
router "weather" "http://wttr.in/ client." none Words
  (λ msg loc, weather.get_weather_by_location loc >>=
              pure ∘ list.singleton ∘ privmsg msg.subject)

end ircbot_external