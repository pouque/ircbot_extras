import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
open types support parser parsing

namespace ircbot_external.capital

def CorrectCapitalCommand : parser string := do
  parsing.tok "\\capital", many_char1 parsing.WordChar

def capital_func (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    args := [subject], text := text } :=
  match run_string CorrectCapitalCommand text with
  | sum.inr place :=
    let answer :=
      match place with
      | "world" := "Krasnoyarsk"
      | "мир" := "Krasnoyarsk is the capital of the world"
      | "2ch" := "/b/"
      | "russia" := "dno"
      | _ := "I don't know"
      end in
    [privmsg subject answer]
  | sum.inl _ := []
  end
| _ := []
end

def capital : bot_function :=
  { name := "capital",
    syntax := some "\\capital [place]",
    description := "Identifies the capital of something!",
    func := functor.map capital_func }

end ircbot_external.capital