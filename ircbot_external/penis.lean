import ircbot.router
open types support parser

namespace ircbot_external

namespace penis
  def minimal_length := 8
  def maximal_length := 30
end penis

def nat.to_bool : ℕ → bool
|    0    := tt
| (n + 1) := ff

def hash (s : string) : nat :=
list.foldl (+) 0 (char.to_nat <$> s.to_list)

def penis : bot_function :=
router "penis" "Measures the penis." "Measures the penis." Word
  (λ msg nick,
    let length := penis.minimal_length + hash nick % penis.maximal_length in
    pure [ privmsg msg.subject $ sformat! "{nick} has {length} cm" ])
  [ message.privmsg ]

def jew : bot_function :=
router "jew" "Detect jews." "Detect jews." Word
  (λ msg nick,
    pure [ privmsg msg.subject
      (if nat.to_bool (hash nick % 2) then
        sformat! "{nick} is jew!"
      else sformat! "{nick} is not jew.") ])
  [ message.privmsg ]

end ircbot_external