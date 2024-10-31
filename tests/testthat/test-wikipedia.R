# test_that("multiplication works", {
#   a <- "GCATGCG" |> strsplit("") |> unlist()
#   b <- "GATTACA" |> strsplit("") |> unlist()
#   align_phones(a, b, indel_create = -1)
# })
#
#
# # phone_match_exact
# # phone_match_exact
# new_phone_match_from_matrix <- function(s) {
#   # A 	G 	C 	T
#   s <- c(
#     10, 	-1, 	-3, -4,
#     -1, 	 7, 	-5, -3,
#     -3, 	-5, 	 9,  0,
#     -4, 	-3, 	 0,  8
#   )
#   m <- matrix(s, byrow = TRUE, nrow = sqrt(length(s)))
#   function(x, y, match = NA, mismatch = NA) {
#
#   }
# }
