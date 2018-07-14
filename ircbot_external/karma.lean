import ircbot.types ircbot.support ircbot.parsing ircbot.unicode
import sum
import data.buffer.parser
open types support parser

namespace ircbot_external.karma

def db_provider := "sqlite3"

inductive database
| conn : io.proc.child → database

def return_db (filename : string) : io database :=
  database.conn <$>
    io.proc.spawn { cmd := db_provider,
                    args := [filename, "-batch"],
                    stdin := io.process.stdio.piped,
                    stdout := io.process.stdio.piped }


namespace database
  def send (req : string) : database → io unit
  | (database.conn child) := io.fs.put_str_ln child.stdin req

  def get_line : database → io string
  | (database.conn child) := do
    buff ← io.fs.get_line child.stdout,
    pure $ option.get_or_else (unicode.utf8_to_string buff) ""

  def get_with_parser {α : Type} (p : parser α) : database → io α
  | (database.conn child) := do
    buff ← io.fs.get_line child.stdout,
    let s := option.get_or_else (unicode.utf8_to_string buff) "",
    sum.either (run_string (p <* optional parsing.Nl) s) io.fail pure

  def get_int := get_with_parser parsing.Integer
  def get_nat := get_with_parser parsing.Number
  def get_bool := get_with_parser $
    (ch '0' >> pure ff) <|> (ch '1' >> pure tt)
end database

def CorrectKarmaCommand : parser string := do
  parsing.tok "\\karma", many_char1 parsing.WordChar

def karma_func (db : database) (input : irc_text) : io (list irc_text) :=
match input with
| irc_text.parsed_normal
  { object := some object, type := message.privmsg,
    args := [subject], text := text } :=
    match run_string CorrectKarmaCommand text with
    | (sum.inr nick) := do
      db.send $ sformat! "SELECT EXISTS(SELECT nickname FROM karma WHERE nickname == '{nick}');",
      exists_nick ← db.get_bool,
      if exists_nick then do
        db.send $ sformat! "SELECT value FROM karma WHERE nickname == '{nick}';",
        value ← db.get_int,
        pure [privmsg subject $ sformat! "{nick} has {value} karma points"]
      else do
        db.send $ sformat! "INSERT INTO karma (nickname, value) VALUES ('{nick}', 0);",
        pure [privmsg subject $ sformat! "{nick} has 0 karma points"]
    | _ := pure []
    end
| _ := pure []
end

def karma (db : database) : bot_function :=
  { name := "karma",
    syntax := some "\\karma [nickname]",
    description := "Measures the KARMA.",
    func := λ input_io, input_io >>= (karma_func db)}

end ircbot_external.karma
