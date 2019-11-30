import ircbot.router
open types support parser

namespace ircbot_external

namespace penis
  def minimal_length := 8
  def maximal_length := 14
end penis

def penis : bot_function :=
router "penis" "Measures the penis." "Measures the penis." Word
  (λ msg nick,
    let length :=
      penis.minimal_length +
        (list.foldl (+) 0 $ char.to_nat <$> nick.to_list) %
        penis.maximal_length in
    pure [ privmsg msg.subject $ sformat! "{nick} has {length} cm" ])
  [ message.privmsg ]

end ircbot_external