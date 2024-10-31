wiscbet_to_ipa_rules <- c(
  "i"  = "i",       # beat
  "I"  = "\u026a",  # bit
  "eI" = "e",       # bait
  "E"  = "\u025b",  # bet
  "ae" = "\u00e6",  # bat
  "^"  = "\u028c",  # but
  "3^" = "\u025d",  # Bert
  "4"  = "\u0259",  # comma: unstressed, neutral vowel
  "4^" = "\u025a",  # letter: unstressed, neutral r-colored vowel
  "@"  = "\u0251",  # bot
  "oU" = "o",       # boat
  "c"  = "\u0254",  # bought
  "u"  = "u",       # boot
  "U"  = "\u028a",  # book
  # "3"  = "\u025c",  # [unclear]
  # "a"  = "a",       # [unclear]
  # "D"  = "\u0252",  # [unclear]
  "@I" = "a\u026a", # bite
  "@U" = "a\u028a", # bout
  "cI" = "\u0254\u026a",  # boyd

  "p" = "p", "b" = "b", "m" = "m",
  "t" = "t", "d" = "d", "n" = "n",
  "k" = "k", "g" = "g", "ng"= "\u014b",
  "tsh" = "t\u0283", "dzh" = "d\u0292", # cheap, jeep

  "f"  = "f", "v"  = "v",
  "th" = "\u03b8", "dh" = "\u00f0", # mouth, mouthe
  "s"  = "s", "z"  = "z",
  "sh" = "\u0283", "zh" = "\u0292", # bash, beige
  "h" = "h",

  "j" = "j", "w" = "w",
  "r" = "r", "l" = "l",
  "." = ".", " " = " ", "-" = "-"
)

#' @export
wiscbet_to_ipa <- function(...) {
  xs <- c(...)
  unname(wiscbet_to_ipa_rules[xs])
}

#' @export
ipa_to_wiscbet <- function(...) {
  xs <- c(...)
  inv_rules <- names(wiscbet_to_ipa_rules)
  names(inv_rules) <- wiscbet_to_ipa_rules
  unname(inv_rules[xs])
}

