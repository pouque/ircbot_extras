import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
open types support parser

namespace ircbot_external.penis

def CorrectPenisCommand : parser string := do
  parsing.tok "\\penis", many_char1 parsing.WordChar

def penis_func (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some object, type := message.privmsg,
    args := [subject], text := text } :=
    match run_string CorrectPenisCommand text with
    | (sum.inr nick) :=
      let length := 8 + (list.foldl (+) 0 (list.map char.to_nat nick.to_list)) % 14 in
      [privmsg subject $ sformat! "{nick} has {length} cm"]
    | _ := []
    end
| _ := []
end

def penis : bot_function :=
  { name := "penis",
    syntax := some "\\penis [nickname]",
    description := "Measures the penis.",
    func := functor.map penis_func }

end ircbot_external.penis