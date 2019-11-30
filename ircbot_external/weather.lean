import ircbot.router
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

def weather : bot_function :=
let emit (subject : string) : string → list irc_text :=
list.singleton ∘ privmsg subject ∘ string.trim char.is_whitespace in
router "weather" "http://wttr.in/ client." none Words
  (λ msg loc, emit msg.subject <$> weather.get_weather_by_location loc)
  [ message.privmsg ]

end ircbot_external