import ircbot.types ircbot.support ircbot.parsing ircbot.effects ircbot.datetime
import data.buffer.parser system.random
open types support parser parsing

namespace ircbot_external.

namespace sieg
  def trusted_bots := [ "dim13" ]

  structure greet :=
  (greeting : string) (my_nickname : string)
  (dest : string) (channel : string)

  def generate_greet (g : greet) : list irc_text :=
  if g.dest ∈ trusted_bots then
  [ privmsg g.channel $ sformat! "Alarm! Bot ITC! {g.dest} is bot!" ]
  else if g.dest ≠ g.my_nickname then
  [ privmsg g.channel $ sformat! "{g.dest}, {g.greeting}! o/" ]
  else []

  def greet_at_join (my_nickname : string) (exceptions : list string)
    (greeting : string) : irc_text → list irc_text
  | (irc_text.parsed_normal
     { object := some ~nick!ident, type := message.join,
       args := channel :: _, text := _ }) :=
    if nick ∉ exceptions then
      generate_greet
        { greeting := greeting,
          my_nickname := my_nickname,
          dest := nick,
          channel := channel }
    else []
  | _ := []

  instance : inhabited (io string) :=
  { default := io.fail "empty string" }

  def get_greeting (greetings : list string) (my_nickname : string) : io string := do
    date ← (λ x, option.get_or_else x datetime.null_date) <$> effects.get_date,
    match date.weekday with
    | datetime.day_of_week.saturday := pure "шаббат шалом"
    | _ := greetings.get <$> (io.rand 0 greetings.length)
    end

  protected def sieg_func (greetings : list string) (exceptions : list string)
    (my_nickname : string) (text : irc_text) : io (list irc_text) := do
    greeting ← get_greeting greetings my_nickname,
    pure $ greet_at_join my_nickname exceptions greeting text

  def GrussCommand : parser string := do
    parsing.tok "\\gruß", many_char1 parsing.WordChar

  protected def gruss_func (greetings : list string) (my_nickname : string)
    (text : irc_text) : io (list irc_text) := do
    match text with
    | (irc_text.parsed_normal
       { object := some ~nick!ident, type := message.privmsg,
         args := [channel], text := text }) :=
      match run_string GrussCommand text with
      | sum.inr nick := do
        greeting ← get_greeting greetings my_nickname,
        pure $ generate_greet
          { greeting := greeting,
            my_nickname := my_nickname,
            dest := nick,
            channel := channel }
      | sum.inl _ := pure []
      end
    | _ := pure []
    end

  def GreetingsFormat :=
  sep_by (Nl >> many' Nl) WordNotNl <* (many' Nl)

  def read_greetings (path : string) : io (list string) := do
    buff ← io.fs.read_file path,
    match run_string GreetingsFormat buff.to_string with
    | sum.inr v := pure v
    | sum.inl er := io.fail $ sformat! "syntax error in {path}:\n{er}"
    end
end sieg

def sieg (greetings : list string) (exceptions : list string)
         (my_nickname : string) : bot_function :=
  { name := "sieg",
    syntax := none,
    description := "Greets at join.",
    func := sieg.sieg_func greetings exceptions my_nickname }

def gruss (greetings : list string) (my_nickname : string) : bot_function :=
  { name := "Gruß",
    syntax := "\\gruß [nickname]",
    description := "Sends greeting.",
    func := sieg.gruss_func greetings my_nickname }

end ircbot_external
