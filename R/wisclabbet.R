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

cmubet_to_ipa_rules <- c(
  "IY" = "i",       # beat
  "IH" = "\u026a",  # bit
  "EY" = "e",       # bait
  "EH" = "\u025b",  # bet
  "AE" = "\u00e6",  # bat
  "AH" = "\u0259",  # comma: unstressed, neutral vowel
  "ER" = "\u025a",  # letter: unstressed, neutral r-colored vowel
  "AA" = "\u0251",  # bot
  "OW" = "o",       # boat
  "AO" = "\u0254",  # bought
  "UW" = "u",       # boot
  "UH" = "\u028a",  # book
  "AY" = "a\u026a", # bite
  "AW" = "a\u028a", # bout
  "OY" = "\u0254\u026a",  # boyd

  "P" = "p", "B" = "b", "M"  = "m",
  "T" = "t", "D" = "d", "N"  = "n",
  "K" = "k", "G" = "g", "NG" = "\u014b",
  "CH" = "t\u0283", "JH" = "d\u0292", # cheap, jeep

  "F"  = "f", "V"  = "v",
  "TH" = "\u03b8", "DH" = "\u00f0", # mouth, mouthe
  "S"  = "s", "Z"  = "z",
  "SH" = "\u0283", "ZH" = "\u0292", # bash, beige
  "HH" = "h",

  "Y" = "j", "W" = "w",
  "R" = "r", "L" = "l",
  "." = ".", " " = " ", "-" = "-"
)

# AH means schwa or carot depending on stress number but then we have
# the same name repeated twice. So, this ..._full version has the repeated
# names.
cmubet_to_ipa_rules_full <- c(
  cmubet_to_ipa_rules,
  "AH" = "\u028c",  # but
  "ER" = "\u025d"   # Bert
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

#' @export
cmubet_to_ipa <- function(...) {
  xs <- c(...)
  unname(cmubet_to_ipa_rules[xs])
}

#' @export
ipa_to_cmubet <- function(...) {
  xs <- c(...)
  inv_rules <- names(cmubet_to_ipa_rules_full)
  names(inv_rules) <- cmubet_to_ipa_rules_full
  unname(inv_rules[xs])
}

