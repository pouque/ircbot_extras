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

  def CorrectWeatherCommand : parser string :=
  parsing.tok "\\weather" >> many_char1 (sat $ function.const char true)
end weather

def weather : bot_function :=
  { name := "weather",
    syntax := none,
    description := "http://wttr.in/ client.",
    func := λ input,
      match input with
      | irc_text.parsed_normal
        { object := some object, type := message.privmsg,
          args := [subject], text := text } := 
        match run_string weather.CorrectWeatherCommand text with
        | (sum.inr loc) := do
          res ← weather.get_weather_by_location loc,
          pure [ privmsg subject res ]
        | _ := pure []
        end
      | _ := pure []
      end }

end ircbot_external