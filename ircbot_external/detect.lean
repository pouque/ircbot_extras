import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
open types support parser

namespace ircbot_external

namespace detect
  def one := char.of_nat 1

  def priv_channel := "#chlor"

  def CorrectClientCommand : parser string := do
    parsing.tok "\\client", many_char1 parsing.WordChar

  def CorrectInfo : parser string := do
    ch one, parsing.tok "VERSION",
    info ← many_char1 $ sat (≠ one),
    ch one, pure info

  protected def detect_func (exceptions : list string) : irc_text → list irc_text
  | (irc_text.parsed_normal
      { object := some ~nick!ident, type := message.join,
        args := channel :: _, text := _ }) :=
    if channel = priv_channel ∧ nick ∉ exceptions ∧ ¬matrix nick then
      [ privmsg nick $ sformat! "{one}VERSION{one}" ]
    else []
  | (irc_text.parsed_normal
      { object := some ~nick!ident, type := message.notice,
        args := _, text := text }) :=
    match run_string CorrectInfo text with
    | (sum.inr info) :=
      [ notice priv_channel $ sformat! "{nick} is using {info}" ]
    | _ := []
    end
  | _ := []

  protected def client_func : irc_text → list irc_text
  | (irc_text.parsed_normal
      { object := _, type := message.privmsg,
        args := _, text := text }) :=
    match run_string CorrectClientCommand text with
    | (sum.inr nick) :=
      [ privmsg nick $ sformat! "{one}VERSION{one}" ]
    | _ := []
    end
  | _ := []
end detect

def detect (exceptions : list string) : bot_function :=
  { name := "detect",
    syntax := none,
    description := "Detects user.",
    func := pure ∘ detect.detect_func exceptions }

def client : bot_function :=
  { name := "client",
    syntax := some "\\client [nickname]",
    description := "Info about users client.",
    func := pure ∘ detect.client_func }

end ircbot_external