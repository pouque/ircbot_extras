import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
import data.buffer
open types support parser parsing

namespace ircbot_external.capital

def reserved := ['\n', char.of_nat 13, ',', '"']
def Nl :=
ch (char.of_nat 10) <|>
ch (char.of_nat 13) <|>
(ch (char.of_nat 13) >> ch (char.of_nat 10))
def Word := many_char1 $ sat (λ c, list.all reserved (≠ c))
def WrappedWord := ch '"' >> Word <* ch '"'

def Country := do
  country ← WrappedWord, ch ',',
  capital ← WrappedWord,
  pure (country, capital)

def CountriesFormat :=
sep_by (Nl >> many' Nl) Country <* (many' Nl)

def CorrectCapitalCommand : parser string := do
  parsing.tok "\\capital", Word

def lookup (target : string) : list (string × string) → option string
| [] := none
| ((key, value) :: tail) :=
  if key = target then some value else lookup tail

def capital_pure_func (db : list (string × string)) (input : irc_text) : list irc_text :=
match input with
| irc_text.parsed_normal
  { object := some ~nick!ident, type := message.privmsg,
    args := [subject], text := text } :=
  match run_string CorrectCapitalCommand text with
  | sum.inr place :=
    let answer := option.get_or_else (lookup place db) "I don't know." in
    [privmsg subject answer]
  | sum.inl _ := []
  end
| _ := []
end

def capital_func (db : string) (input_io : io irc_text) : io (list irc_text) := do
  buff ← io.fs.read_file db,
  input ← input_io,
  match run_string CountriesFormat buff.to_string with
  | sum.inr v := pure $ capital_pure_func v input
  | sum.inl er := io.put_str_ln er >> pure []
  end

def capital (db : string) : bot_function :=
  { name := "capital",
    syntax := some "\\capital [place]",
    description := "Identifies the capital of something!",
    func := capital_func db }

end ircbot_external.capital