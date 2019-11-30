import ircbot.router
open types support parser

namespace ircbot_external

def moveton.template (subject obj : string) :=
[ privmsg subject $ sformat! "{obj} — моветон."]

def moveton : bot_function :=
router "moveton" "Finds moveton." "\\moveton [something]" Words
  (λ msg, pure ∘ moveton.template msg.subject) [ message.privmsg ]

end ircbot_external