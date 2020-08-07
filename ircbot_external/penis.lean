import ircbot.router
open types support parser

def string.hash (s : string) : nat :=
list.foldl (+) 0 (char.to_nat <$> s.to_list)

def list.get_by_hash {α : Type} [inhabited α] (xs : list α) (s : string) : α :=
xs.get (s.hash % xs.length)

namespace ircbot_external

namespace penis
  def minimal_length := 8
  def maximal_length := 30
end penis

def genders :=
  ["agender", "cis man", "androgyne", "androgynous", "bigender", "cis female",
   "FTM", "cis woman", "cisgender female", "cisgender male", "cisgender man",
   "female to male", "cis male", "gender fluid", "gender nonconforming",
   "gender questioning", "gender variant", "genderqueer", "intersex",
   "male to female", "non-binary", "neither", "neutrois", "other",
   "pangender", "trans female", "trans male", "trans man", "trans person",
   "trans woman", "transexual", "transexual female", "transexual male",
   "transexual man", "transexual person", "transexual woman", "transgender female",
   "transgender male", "transgender man", "transgender person", "transgender woman",
   "transmasculine", "two-spirit", "cisgender woman"]

def ideologies :=
  ["left anarchist", "liberal", "right anarchist", "libertarian",
   "eurasian", "communist", "fascist", "populist", "social democrat",
   "syndicalist", "nationalist", "schizophrenic"]

def colours := ["nigger", "american nigger", "yellow-cheeked", "asian", "mexican", "bashkir", "buryat", "tatarstanian", "white"]
def sizes := ["extremly fat", "abnormally fat", "very fat", "fat", "normal weight", "extremly thin", "thin", "skinny"]
def look := ["very sexy", "sexy", "pretty", "normal looking", "unremarkable", "ugly", "freak", "retarded looking"]
def religious :=
  ["orthodox", "atheist", "catholic", "muslim",
   "judaist", "judaist", "agnostic", "pagan", "ukrop", "bulbash"]

def nat.to_bool : ℕ → bool
|    0    := tt
| (n + 1) := ff

def penis : bot_function :=
router "penis" "Measures the penis." "\\penis <nick>" Word
  (λ msg nick,
    let length := penis.minimal_length + nick.hash % penis.maximal_length in
    pure [ privmsg msg.subject $ sformat! "{nick} has {length} cm" ])
  [ message.privmsg ]

def jew : bot_function :=
router "jew" "Detect jews." "\\jew <nick>" Word
  (λ msg nick,
    pure [ privmsg msg.subject
      (if nat.to_bool (nick.hash % 2) then
        sformat! "{nick} is jew!"
      else sformat! "{nick} is not jew.") ])
  [ message.privmsg ]

def specs := [sizes, look, colours, genders, ideologies, religious]
def get_profile (nick : string) :=
string.intercalate " " $ (λ xs, list.get_by_hash xs nick) <$> specs

def profile : bot_function :=
router "profile" "Return profile." "\\profile <nick>" Word
  (λ msg nick, let profile := get_profile nick in
    pure [ privmsg msg.subject (sformat! "{nick}: {profile}") ])
  [ message.privmsg ]

end ircbot_external
