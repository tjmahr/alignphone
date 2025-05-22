test_that("g characters are the same", {
  a <- c("g", "a")
  b <- c("ɡ", "a")
  r <- align_phones(
    c("g", "i", "s"),
    c("ɡ", "i", "z"),
    fun_match = phone_match_partial
  )

  expect_equal(r$scores[1], 1.0)
})
