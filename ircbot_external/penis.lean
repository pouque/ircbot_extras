import ircbot.router
open types support parser

namespace ircbot_external

namespace penis
  def minimal_length := 8
  def maximal_length := 30

  def genders :=
    ["Agender", "Androgyne", "Androgynous", "Bigender", "Cis",
     "Cis Female", "Cis Male", "Cis Man", "Cis Woman", "Cisgender",
     "Cisgender Female", "Cisgender Male", "Cisgender Man", "Cisgender Woman",
     "Female to Male", "FTM", "Gender Fluid", "Gender Nonconforming",
     "Gender Questioning", "Gender Variant", "Genderqueer", "Intersex",
     "Male to Female", "MTF", "Neither", "Neutrois", "Non-binary", "Other",
     "Pangender", "Trans", "Trans Female", "Trans Male", "Trans Man", "Trans Person",
     "Trans Woman", "Transexual", "Transexual Female", "Transexual Male",
     "Transexual Man", "Transexual Person", "Transexual Woman", "Transgender Female",
     "Transgender Male", "Transgender Man", "Transgender Person", "Transgender Woman",
     "Transmasculine", "Two-spirit"]
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

def gender : bot_function :=
router "gender" "Check gender." "Check gender." Word
  (λ msg nick,
    let idx := hash nick % penis.genders.length in
    pure [ privmsg msg.subject (penis.genders.get idx) ])
  [ message.privmsg ]

end ircbot_external