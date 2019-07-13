import ircbot.types ircbot.support ircbot.parsing ircbot.effects ircbot.datetime
import data.buffer.parser system.random
open types support parser parsing

namespace list
  def get {α : Type} [inhabited α] (l : list α) (n : ℕ) : α :=
  list.head (list.drop n l)
end list

namespace ircbot_external.sieg

def trusted_bots := [ "dim13", "Netherlands", "Qubick", "jircbot" ]

def sieg_func_pure (my_nickname : string) (greeting : string) : irc_text → list irc_text
| (irc_text.parsed_normal
   { object := some ~nick!ident, type := message.join,
     args := channel :: _, text := _ }) :=
  if nick ∈ trusted_bots then
  [ privmsg channel $ sformat! "Alarm! Bot ITC! {nick} is bot!" ]
  else if nick ≠ my_nickname then
  [ privmsg channel $ sformat! "{nick}, {greeting}! o/" ]
  else []
| _ := []

instance : inhabited (io string) :=
{ default := io.fail "empty string" }

def sieg_func (greetings : list string) (my_nickname : string) (raw_text : io irc_text) :
  io (list irc_text) := do
  date ← (λ x, option.get_or_else x datetime.null_date) <$> effects.get_date,
  text ← raw_text,
  greeting ←
    (io.rand 0 $ greetings.length - 1) >>= pure ∘ greetings.get,
    --match date.weekday with
    --| datetime.day_of_week.saturday := pure "шаббат шалом"
    --| _ := (io.rand 0 $ greetings.length - 1) >>= pure ∘ list.get greetings
    --end,
  pure $ sieg_func_pure my_nickname greeting text

def sieg (greetings : list string) (my_nickname : string) : bot_function :=
  { name := "sieg",
    syntax := none,
    description := "SIEG HEIL at join.",
    func := sieg_func greetings my_nickname }

def GreetingsFormat :=
sep_by (Nl >> many' Nl) WordNotNl <* (many' Nl)

def read_greetings (path : string) : io (list string) := do
  buff ← io.fs.read_file path,
  match run_string GreetingsFormat buff.to_string with
  | sum.inr v := pure v
  | sum.inl er := io.fail $ sformat! "syntax error in {path}:\n{er}"
  end

end ircbot_external.sieg

def main : io unit := do
  greetings ← ircbot_external.sieg.read_greetings "greetings.txt",
  idx ← io.rand 0 (greetings.length - 1),
  io.put_str_ln (greetings.get idx)