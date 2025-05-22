test_that("matches results from old alignment software", {
  those_eat_those_hotdogs_soon <- clean_old_alignment_result(
    "dh-oU-z i-t dh-oU-z h-@-t--d-c-g-z  s-u-n"
  )
  hot_dogs <- clean_old_alignment_result(
    "--------------------h-@-t  d-c-g-z------"
  )
#
#   r <- align_phones(
#     those_eat_those_hotdogs_soon |> wiscbet,
#     hot_dogs,
#     phone_match_partial
#   )
#
#   expect_equal(
#     r$a_alignment,
#     c("dh", "oU", "z", " ", "i", "t", " ", "dh", "oU", "z", " ",
#       "h", "@", "t", "-", "d", "c", "g", "z", " ", "s", "u", "n")
#   )
#
#   expect_equal(
#     r$b_alignment,
#     c("-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "h",
#       "@", "t", " ", "d", "c", "g", "z", "-", "-", "-", "-")
  # )
})
