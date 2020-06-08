import ircbot.types ircbot.support ircbot.parsing
open types support parser

namespace ircbot_external

namespace justice
  def truth := privmsg "#chlor" "Вы все геи."

  def valid (c : char) : Prop :=
  char.is_alpha c ∨ (c.val ≥ 'А'.val ∧ c.val ≤ 'я'.val)

  instance decidable_valid : decidable_pred valid :=
  begin intro c, delta valid, apply_instance end

  def tolower (c : char) : char :=
  let n := c.to_nat in
  if n ≥ 'A'.val ∧ n ≤ 'Z'.val then char.of_nat (n + 'a'.val - 'A'.val)
  else if n ≥ 'А'.val ∧ n ≤ 'Я'.val then char.of_nat (n + 'а'.val - 'А'.val)
  else c

  def clean (s : string) : list char :=
  tolower <$> s.to_list.filter valid

  def substring {α : Type} [decidable_eq α] (x : list α) : list α → bool
  |    []     := ff
  | (y :: ys) := list.is_prefix_of x (y :: ys) ∨ substring ys

  def triggers := string.to_list <$>
    [ "гей", "гея", "гею", "геем", "гее",
      "геи", "геев", "геям", "геями", "геях",
      "gay", "пердоликс", "perdoliks" ]

  def check (s : string) :=
  let s' := clean s in list.any triggers (λ x, substring x s')
end justice

def justice : bot_function :=
{ name := "justice",
  syntax := none,
  description := "Restores justice in #chlor.",
  func := λ input,
    match input with
    | irc_text.parsed_normal
      { object := some object, type := type,
        args := ["#chlor"], text := text } :=
        if (type = message.privmsg ∨ type = message.notice) ∧
            justice.check text then pure [ justice.truth ]
        else pure []
    | _ := pure []
    end }

end ircbot_external