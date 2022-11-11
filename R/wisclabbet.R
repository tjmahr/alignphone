#' @export
wiscbet_to_ipa <- function(...) {
  xs <- c(...)
  rules <- c(
    "i" = "i",  # beat
    "I" = "ɪ",  # bit
    "eI" = "e", # bait
    "E" = "ɛ",  # bet
    "ae" = "æ", # bat
    "^" = "ʌ",  # but
    "3^" = "ɝ", # Bert
    "4" = "ə",  # comma: unstressed, neutral vowel
    "4^" = "ɚ", # letter: unstressed, neutral r-colored vowel
    "@" = "ɑ",  # bot
    "oU" = "o", # boat
    "c" = "ɔ",  # bought
    "u" = "u",  # boot
    "U" = "ʊ",  # book
    # "3" = "ɜ",  # [unclear]
    # "a" = "a",  # [unclear]
    # "D" = "ɒ",  # [unclear]
    "@I" = "aɪ", # bite
    "@U" = "aʊ", # bout
    "cI"= "ɔɪ",  # boyd

    "p" = "p", "b" = "b", "m" = "m",
    "t" = "t", "d" = "d", "n" = "n",
    "k" = "k", "g" = "g", "ng"= "ŋ",
    "tsh" = "tʃ", "dzh" = "dʒ", # cheap, jeep

    "f"  = "f", "v"  = "v",
    "th" = "θ", "dh" = "ð", # mouth, mouthe
    "s"  = "s", "z"  = "z",
    "sh" = "ʃ", "zh" = "ʒ", # bash, beige
    "h" = "h",

    "j" = "j", "w" = "w",
    "r" = "r", "l" = "l",
    "." = ".", " " = " "
  )
  unname(rules[xs])
}
