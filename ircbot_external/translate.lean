import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
open types support parser

namespace ircbot_external

namespace translate
  def Prefix :=
  str "\\pizdec" <|>
  str "\\cyka" <|>
  str "\\blyad" <|>
  str "\\bljad" <|>
  str "\\nahuj"

  def CorrectTranslateCommand : parser (option string) :=
  Prefix >> optional (parsing.Ws >> parsing.Word)

  protected def translate_func (input : irc_text) : io (list irc_text) :=
  match input with
  | irc_text.parsed_normal
    { object := some ~nick!ident, type := message.privmsg,
      args := [subject], text := text } :=
      let convert := λ (s : string),
        [ privmsg "Konsolechka" $ sformat! "!twi {s}" ] in
      let return_wave := (λ n, sformat! "Wave number {n}!") <$> io.rand in
      match run_string CorrectTranslateCommand text with
      | (sum.inl _) := pure []
      | (sum.inr (some s)) := pure $ convert s
      | (sum.inr none) := convert <$> return_wave
      end
  | _ := pure []
  end
end translate

def translate : bot_function :=
  { name := "retranslator 9000",
    syntax := some "\\pizdec",
    description := "Some pizdec",
    func := translate.translate_func }

end ircbot_external
