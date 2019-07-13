import data.buffer.parser
import ircbot ircbot.base64
import ircbot.modules
import ircbot_external.penis ircbot_external.detect
import ircbot_external.capital ircbot_external.sieg
import ircbot_external.urls

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

def channels := ["#chlor"]

def messages : list irc_text :=
  join <$> channels ++
  [ privmsg "#chlor" "Пруверы правят миром",
    mode bot_nickname "+B" ]

def my_bot_info : bot_info :=
bot_info.mk bot_nickname ident server port

def my_funcs (countries : list (string × string))
  (greetings : list string) (acc : account) : list bot_function :=
  [ modules.ping_pong.ping_pong,
    sasl my_bot_info messages acc,
    modules.print_date.print_date,
    modules.admin.join_channel,
    ircbot_external.penis.penis,
    ircbot_external.detect.detect,
    ircbot_external.detect.client,
    ircbot_external.capital.capital countries,
    ircbot_external.sieg.sieg greetings my_bot_info.nickname,
    ircbot_external.urls.titles,
    relogin ]

def my_bot (countries : list (string × string))
  (greetings : list string) (acc : account) : bot :=
let funcs := my_funcs countries greetings acc in
{ info := my_bot_info,
  funcs := modules.help.help funcs :: funcs,
  unicode_fix := ff }

def countries_file := "countries.csv"
def greetings_file := "greetings.txt"

def main := do
  --db ← ircbot_external.karma.return_db "bot.sqlite",
  args ← io.cmdline_args,
  countries ← ircbot_external.capital.read_db countries_file,
  greetings ← ircbot_external.sieg.read_greetings greetings_file,
  match args with
  | (login :: password :: []) :=
    mk_bot (my_bot countries greetings $ account.mk login password)
  | _ := io.fail "syntax: lean --run file.lean [login] [password]"
  end