test_that("Alignment matches online examples", {
  a <- "GCATGCG" |> strsplit("") |> unlist() |> alignment_utterance()
  a <- "AAAAA" |> strsplit("") |> unlist() |> alignment_utterance()
  b <- "GATTACA" |> strsplit("") |> unlist() |> alignment_utterance()
  align_phones(a, b, indel_create = -1)
})


# phone_match_exact
# phone_match_exact
new_phone_match_from_matrix <- function(s) {
  # A 	G 	C 	T
  s <- c(
    10, 	-1, 	-3, -4,
    -1, 	 7, 	-5, -3,
    -3, 	-5, 	 9,  0,
    -4, 	-3, 	 0,  8
  )
  m <- matrix(s, byrow = TRUE, nrow = sqrt(length(s)))
  function(x, y, match = NA, mismatch = NA) {

  }
}
