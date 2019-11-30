import ircbot.router
open types support parser parsing

namespace ircbot_external

namespace capital
  def reserved := [ '\n', char.of_nat 13, ',', '"' ]
  def Word := many_char1 $ sat (λ c, list.all reserved (≠ c))
  def Quoted := ch '"' >> Word <* ch '"'

  def Country := do
    country ← Quoted, ch ',', capital ← Quoted,
    pure (country, capital)

  def CSV :=
  sep_by (Nl >> many' Nl) Country <* (many' Nl)

  def lookup (target : string) : list (string × string) → option string
  | [] := none
  | ((key, value) :: tail) :=
    if key = target then some value else lookup tail

  def read_db (path : string) : io (list (string × string)) := do
    buff ← io.fs.read_file path,
    match run_string CSV buff.to_string with
    | sum.inr v := pure v
    | sum.inl er := io.fail (sformat! "syntax error in {path}:\n{er}")
    end
end capital

def capital (db : list (string × string)) : bot_function :=
router "capital" "Identifies the capital of something!"
  "\\capital [place]" capital.Word
  (λ msg place, pure
    [ privmsg msg.subject $
        option.get_or_else (capital.lookup place db) "I don’t know." ])
  [ message.privmsg ]

end ircbot_external