import ircbot.types ircbot.support ircbot.parsing
import data.buffer.parser
open types support parser

namespace ircbot_external.urls

def Scheme : parser string :=
str "http://" >> pure "http://" <|>
str "https://" >> pure "https://"

def Url : parser string := do
(++) <$> Scheme <*> many_char1 parsing.WordChar

def delims : list char := [' ', '\t', ',', '.', ';', '|']

def minimal_length := 8
def maximal_length := 14

def get_titles : irc_text → list irc_text
| (irc_text.parsed_normal
    { object := some object, type := message.privmsg,
      args := [subject], text := text }) :=
    list.filter_map (λ word, match run_string Url word with
      | sum.inr url := some $ privmsg subject $ sformat! "URL found: {url}"
      | sum.inl _ := none
    end) $ text.split (∈ delims)
| _ := []

def titles : bot_function :=
  { name := "titles",
    syntax := none,
    description := "Returns titles of urls.",
    func := functor.map get_titles }

end ircbot_external.urls