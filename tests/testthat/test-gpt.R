# These are tests suggested by ChatGPT

test_that("Handles empty sequences", {
  a <- character(0)
  b <- c("G", "C", "A", "T", "G", "C", "U")
  gaps <- c("-", "-", "-", "-", "-", "-", "-")
  result <- align_phones(a, b)

  expect_equal(sum(result$score), -length(b))
  expect_equal(result$a_alignment, gaps)
  expect_equal(result$b_alignment, b)

  result <- align_phones(b, a)
  expect_equal(sum(result$score), -length(b))
  expect_equal(result$a_alignment, b)
  expect_equal(result$b_alignment, gaps)

})

test_that("Symmetric alignment score for swapped sequences", {
  seq1 <- c("G", "A", "T", "T", "A", "C", "A")
  seq2 <- c("G", "C", "A", "T", "G", "C", "U")
  result1 <- align_phones(seq1, seq2)
  result2 <- align_phones(seq2, seq1)

  expect_equal(result1$scores, result1$scores)
})


test_that("Handles single-character sequences", {
  x <- c("A")
  y <- c("G")
  z <- c("T", "A", "C")

  result <- align_phones(x, y)
  expect_equal(result$a_alignment, x)
  expect_equal(result$b_alignment, y)
  expect_equal(result$scores, -1)

  result <- align_phones(x, x)
  expect_equal(result$a_alignment, x)
  expect_equal(result$b_alignment, x)
  expect_equal(result$scores, 1)

  result <- align_phones(x, z)
  expect_equal(result$a_alignment, c("-", "A", "-"))
  expect_equal(result$b_alignment, z)
  expect_equal(sum(result$scores), -1)
})


test_that("Identical sequences result in no gaps", {
  x <- c("G", "A", "T", "T", "A", "C", "A")
  result <- align_phones(x, x)

  expect_equal(result$a_alignment, x)
  expect_equal(result$b_alignment, x)

  expect_equal(sum(result$scores), length(x))
})
