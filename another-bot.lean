import data.buffer.parser
import ircbot ircbot.base64
import ircbot.modules
import ircbot_external.penis ircbot_external.detect
import ircbot_external.capital ircbot_external.sieg
import ircbot_external.karma

open types effects support parsing login
open parser

-- constants
def server : string := "chat.freenode.net"
def port : string := "6667"

def ident : string := "lean"
def bot_nickname : string := "leanbot"
-- constants

theorem bot_nickname_is_correct : bot_nickname.front ≠ '#' :=
begin intros contra, cases contra end

def messages : list irc_text :=
  [join "#lor",
   privmsg "#lor" "Problema v tebe, cyka",
   mode bot_nickname "+B"]

def my_bot_info : bot_info :=
bot_info.mk bot_nickname ident server port

def my_funcs (acc : account) : list bot_function :=
  [modules.ping_pong.ping_pong,
   sasl my_bot_info messages acc,
   modules.print_date.print_date,
   modules.admin.join_channel,
   ircbot_external.penis.penis,
   ircbot_external.detect.detect,
   ircbot_external.detect.client,
   ircbot_external.capital.capital,
   ircbot_external.sieg.sieg my_bot_info.nickname,
   relogin]

def my_bot (acc : account) : bot :=
let funcs := my_funcs acc in
{ info := my_bot_info,
  funcs := modules.help.help funcs :: funcs }

def main := do
  --db ← ircbot_external.karma.return_db "bot.sqlite",
  args ← io.cmdline_args,
  match args with
  | (login :: password :: []) :=
    mk_bot (my_bot $ account.mk login password)
  | _ := io.fail "syntax: lean --run file.lean [login] [password]"
  end
