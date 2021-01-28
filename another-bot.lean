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
def server : string := "irc.quakenet.org"
def port : string := "6667"

def ident : string := "lean"
def bot_nickname : string := "leanbot"
-- constants

meta def cases_trivial : tactic unit :=
`[ intro x, cases x ]

def exceptions := [ "fedor_rus", "fedor" ]

def messages : list irc_text :=
  [ irc_text.parsed_normal
      { type := message.nick,
        text := "",
        args := [bot_nickname] },
    join "#chlor",
    privmsg "#chlor" "Аниме придумал Сатана.",
    mode bot_nickname "+B" ]

def my_bot_info : bot_info :=
bot_info.mk bot_nickname (by cases_trivial) ident server port messages

def my_funcs (countries : list (string × string))
  (greetings : list string) : list bot_function :=
  [ modules.ping_pong.ping_pong,
    modules.print_date.print_date,
    modules.admin.join_channel,
    ircbot_external.penis, ircbot_external.jew, ircbot_external.profile,
    ircbot_external.detect exceptions,
    ircbot_external.client,
    ircbot_external.capital countries,
    ircbot_external.sieg greetings exceptions my_bot_info.nickname,
    ircbot_external.gruss greetings my_bot_info.nickname,
    ircbot_external.urls,
    ircbot_external.moveton,
    ircbot_external.weather,
    relogin ]

def my_bot (countries : list (string × string))
  (greetings : list string) : bot :=
let funcs := my_funcs countries greetings in
{ info := my_bot_info,
  funcs := modules.help.help funcs :: funcs,
  fix := ⟨tt, ff⟩ }

def countries_file := "countries.csv"
def greetings_file := "greetings.txt"

def main := do
  countries ← ircbot_external.capital.read_db countries_file,
  greetings ← ircbot_external.sieg.read_greetings greetings_file,
  mk_bot (my_bot countries greetings) netcat
