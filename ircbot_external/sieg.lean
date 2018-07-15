import ircbot.types ircbot.support ircbot.parsing ircbot.effects ircbot.datetime
import data.buffer.parser
open types support parser

namespace ircbot_external.sieg

def sieg_func_pure (my_nickname : string) (is_schabbat : bool) : irc_text → list irc_text
| (irc_text.parsed_normal
   { object := some ~nick!ident, type := message.join,
     args := channel :: _, text := _ }) :=
  let shabbat_greeting := if is_schabbat then "Schabbat schalouem " else "" in
  if nick ≠ my_nickname then
    [privmsg channel $ sformat! "{nick}, {shabbat_greeting}o/"]
  else []
| _ := []

def sieg_func (my_nickname : string) (raw_text : io irc_text) :
  io (list irc_text) := do
  date ← (λ x, option.get_or_else x datetime.null_date) <$> effects.get_date,
  text ← raw_text,
  let is_schabbat := match date.weekday with
  | datetime.day_of_week.sunday := tt | _ := ff end,
  pure $ sieg_func_pure my_nickname is_schabbat text

def sieg (my_nickname : string) : bot_function :=
  { name := "sieg",
    syntax := none,
    description := "SIEG HEIL at join.",
    func := sieg_func my_nickname }

end ircbot_external.sieg
