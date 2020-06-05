import data.buffer.parser
import ircbot ircbot.base64
import ircbot.modules
import ircbot_external.penis ircbot_external.detect
import ircbot_external.capital ircbot_external.sieg
import ircbot_external.urls ircbot_external.moveton
import ircbot_external.weather ircbot_external.justice

open types effects support parsing login
open parser

-- constants
def server : string := "chat.freenode.net"
def port : string := "6667"

def ident : string := "lean"
def bot_nickname : string := "leanbot"
-- constants

meta def cases_trivial : tactic unit :=
`[ intro x, cases x ]

def channels := [ "#chlor", "#n2o", "#groupoid" ]

def exceptions := [ "fedor_rus", "fedor" ]

def messages : list irc_text :=
  join <$> channels ++
  [ privmsg "#chlor" "Аниме придумал Сатана.",
    mode bot_nickname "+B" ]

def my_bot_info : bot_info :=
bot_info.mk bot_nickname (by cases_trivial) ident server port

def my_funcs (countries : list (string × string))
  (greetings : list string) (acc : account) : list bot_function :=
  [ modules.ping_pong.ping_pong,
    sasl my_bot_info messages acc,
    modules.print_date.print_date,
    modules.admin.join_channel,
    ircbot_external.penis,
    ircbot_external.detect exceptions,
    ircbot_external.client,
    ircbot_external.capital countries,
    ircbot_external.sieg greetings exceptions my_bot_info.nickname,
    ircbot_external.gruss greetings my_bot_info.nickname,
    ircbot_external.urls,
    ircbot_external.moveton,
    ircbot_external.weather,
    ircbot_external.justice,
    relogin ]

def my_bot (countries : list (string × string))
  (greetings : list string) (acc : account) : bot :=
let funcs := my_funcs countries greetings acc in
{ info := my_bot_info,
  funcs := modules.help.help funcs :: funcs,
  fix := ⟨tt, ff⟩ }

def countries_file := "countries.csv"
def greetings_file := "greetings.txt"

def main := do
  args ← io.cmdline_args,
  countries ← ircbot_external.capital.read_db countries_file,
  greetings ← ircbot_external.sieg.read_greetings greetings_file,
  match args with
  | (login :: password :: []) :=
    mk_bot (my_bot countries greetings $ account.mk login password) netcat
  | _ := io.fail "syntax: lean --run file.lean [login] [password]"
  end