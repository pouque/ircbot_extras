import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
open types support parser

namespace ircbot_external.moveton

def MovetonCommand : parser string :=
parsing.tok "\\moveton" >> many_char1 (sat $ function.const char true)

def template (subject obj : string) :=
[ privmsg subject $ sformat! "{obj} — моветон."]

def moveton_func (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some object, type := message.privmsg,
    args := [subject], text := text } :=
    sum.rec_on (run_string MovetonCommand text) (λ _, []) (template subject)
| _ := []
end

def moveton : bot_function :=
  { name := "moveton",
    syntax := some "\\moveton [nickname]",
    description := "Find moveton.",
    func := functor.map moveton_func }

end ircbot_external.moveton