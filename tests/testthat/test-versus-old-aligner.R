test_that("Easy alignment case", {
  those_eat_those_hotdogs_soon <- clean_old_alignment_result(
    "dh-oU-z i-t dh-oU-z h-@-t--d-c-g-z  s-u-n"
  )
  hot_dogs <- clean_old_alignment_result(
    "--------------------h-@-t  d-c-g-z------"
  )

  r0 <- align_phones(
    those_eat_those_hotdogs_soon,
    hot_dogs,
    phone_match_exact
  )

  expect_equal(
    r0$a_alignment,
    c("dh", "oU", "z", " ", "i", "t", " ", "dh", "oU", "z", " ",
      "h", "@", "t", "-", "d", "c", "g", "z", " ", "s", "u", "n")
  )

  expect_equal(
    r0$b_alignment,
    c("-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "h",
      "@", "t", " ", "d", "c", "g", "z", "-", "-", "-", "-")
  )


  r <- align_phones(
    wiscbet_to_ipa(those_eat_those_hotdogs_soon),
    wiscbet_to_ipa(hot_dogs),
    phone_match_partial
  )

  expect_equal(
    ipa_to_wiscbet(r$a_alignment),
    c("dh", "oU", "z", " ", "i", "t", " ", "dh", "oU", "z", " ",
      "h", "@", "t", "-", "d", "c", "g", "z", " ", "s", "u", "n")
  )

  expect_equal(
    ipa_to_wiscbet(r$b_alignment),
    c("-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "h",
      "@", "t", " ", "d", "c", "g", "z", "-", "-", "-", "-")
  )

})
