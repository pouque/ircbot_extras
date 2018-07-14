import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
open types support parser

namespace ircbot_external.sieg

def sieg_func (my_nickname : string) : irc_text → list irc_text
| (irc_text.parsed_normal
   { object := some ~nick!ident, type := message.join,
     args := channel :: _, text := _ }) :=
  if nick ≠ my_nickname then
    [privmsg channel $ sformat! "{nick}, o/"]
  else []
| _ := []

def sieg (my_nickname : string) : bot_function :=
  { name := "sieg",
    syntax := none,
    description := "SIEG HEIL at join.",
    func := functor.map $ sieg_func my_nickname }

end ircbot_external.sieg
