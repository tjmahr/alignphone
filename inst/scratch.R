devtools::load_all()

vec_drop <- function(xs, drop) {
  xs[which(! xs %in% drop)]
}
xs <- letters
drop <- 1:3
letters |> vec_drop(1:3) |> vec_drop(NA) |> vec_drop("a")

parse_legacy_alignment_chunk <- function(x) {
  xs <- x |> stringr::str_split("\n") |> unlist()
  utt1 <- xs[1]
  utt2 <- xs[2]
  phon1 <- xs[4] |> clean_old_alignment_result() |> vec_drop(" ") |> wiscbet_to_ipa()
  phon2 <- xs[6] |> clean_old_alignment_result() |> vec_drop(" ") |> wiscbet_to_ipa()
  list(
    a = alignment_utterance(phon1, utt1),
    b = alignment_utterance(phon2, utt2)
  )
}

x <- "Make a birdhouse
make apple sauce

m-eI-k -----4-  b-3^-d-h-@U-s
| |  ||     |  |            |
m-eI-k ae-p-4-l s-c---------s
"

data <- parse_legacy_alignment_chunk(x)
align_phones(data$a, data$b, phone_match_exact)
align_phones(data$a, data$b, phone_match_partial)

parse_legacy_alignment_chunk(x) |>
  c()
  do.call(align_phones, args = _)


clean_old_alignment_result()
