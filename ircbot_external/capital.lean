import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
import data.buffer
open types support parser parsing

namespace ircbot_external

namespace capital
  def reserved := ['\n', char.of_nat 13, ',', '"']
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

  private def lookup (target : string) : list (string × string) → option string
  | [] := none
  | ((key, value) :: tail) :=
    if key = target then some value else lookup tail

  protected def capital_func (db : list (string × string)) (input : irc_text) : list irc_text :=
  match input with
  | irc_text.parsed_normal
    { object := some ~nick!ident, type := message.privmsg,
      args := [subject], text := text } :=
    match run_string CorrectCapitalCommand text with
    | sum.inr place :=
      let answer := option.get_or_else (lookup place db) "I don't know." in
      [ privmsg subject answer ]
    | sum.inl _ := []
    end
  | _ := []
  end

  def read_db (path : string) : io (list (string × string)) := do
    buff ← io.fs.read_file path,
    match run_string CountriesFormat buff.to_string with
    | sum.inr v := pure v
    | sum.inl er := io.fail $ sformat! "syntax error in {path}:\n{er}"
    end
end capital

def capital (db : list (string × string)) : bot_function :=
  { name := "capital",
    syntax := some "\\capital [place]",
    description := "Identifies the capital of something!",
    func := functor.map $ capital.capital_func db }

end ircbot_external