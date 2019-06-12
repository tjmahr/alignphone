
#' Align two sequences of phonemes using global alignment
#'
#' @param a,b two sequences of phonemes to align
#' @param fun_match function to compute the similarity scores for each phoneme
#' @param indel score (penalty) to assign to an insertion or deletion (indel)
#' @return a list with information about the matching
#' @export
align_phones <- function(a, b, fun_match = phone_match_exact, indel = -1) {
  grid <- align_grid_setup(a, b, fun_match = fun_match, indel = indel)
  aligned <- align_grid_walk(grid, fun_match = fun_match, indel = indel)

  # put the original strings at the front of the results
  results <- list(a = a, b = b)
  results[names(aligned)] <- aligned

  structure(results, class = c("phone_alignment", "list"))
}


#' @export
print.phone_alignment <- function(x, ...) {
  matches <- x$scores
  lengths1 <- nchar(x$a_alignment)
  lengths2 <- nchar(x$b_alignment)
  lengths <- pmax(lengths1, lengths2)

  pad_top <- x$a_alignment %>%
    purrr::map2_chr(
      lengths - lengths1,
      function(x, y) paste0(rep(" ", y), collapse = "")
    ) %>%
    paste0(x$a_alignment, ., collapse = " ")

  pad_bot <- x$b_alignment %>%
    purrr::map2_chr(
      lengths - lengths2,
      function(x, y) paste0(rep(" ", y), collapse = "")
    ) %>%
    paste0(x$b_alignment, ., collapse = " ")

  marks <- purrr::list_along(x$aligners) %>%
    purrr::map2(lengths, function(x, y) paste0(rep(" ", y), collapse = "")) %>%
    purrr::map2(x$aligners, function(x, y) paste0(y, x)) %>%
    unlist() %>%
    paste0(collapse = "")


  cat(pad_top, marks, pad_bot, sep = "\n")
  invisible(x)
}

align_grid_setup <- function(a, b, fun_match = check, indel = -1) {
  grid <- matrix(
    nrow = 1 + length(a),
    ncol = 1 + length(b)
  )

  # setup insertions
  grid[1, ] <- seq(0, length.out = ncol(grid), by = -1)
  grid[, 1] <- seq(0, length.out = nrow(grid), by = -1)
  rownames(grid) <- c("", a)
  colnames(grid) <- c("", b)

  # value in each cell is either a match from the diagonal or an indel
  for (i in seq(2, nrow(grid))) {
    for (j in seq(2, ncol(grid))) {
      .match  <- grid[i - 1, j - 1] + fun_match(c("", a)[i], c("", b)[j])
      .delete <- grid[i - 1, j    ] + indel
      .insert <- grid[i    , j - 1] + indel
      grid[i, j] <- max(c(.match, .delete, .insert))
    }
  }
  grid
}


align_grid_walk <- function(grid, fun_match = check, indel = -1) {
  grid_a <- rownames(grid)
  grid_b <- colnames(grid)
  alignment_a <- character(0)
  alignment_b <- character(0)
  i <- length(grid_a)
  j <- length(grid_b)

  # https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
  while (i > 1 || j > 1) {
    this_check <- fun_match(grid_a[i], grid_b[j])
    not_top_row <- i > 0
    not_left_row <- j > 0
    not_corner <- not_top_row && not_left_row

    diagonal <-  not_corner && grid[i, j] == (grid[i - 1, j - 1] + this_check)
    upper    <- not_top_row && grid[i, j] == (grid[i - 1, j] + indel)

    if (diagonal) {
      alignment_a <- c(grid_a[i], alignment_a)
      alignment_b <- c(grid_b[j], alignment_b)
      i <- i - 1
      j <- j - 1
    } else if (upper) {
      alignment_a <- c(grid_a[i], alignment_a)
      alignment_b <- c("-", alignment_b)
      i <- i - 1
    } else {
      alignment_a <- c("-", alignment_a)
      alignment_b <- c(grid_b[j], alignment_b)
      j <- j - 1
    }
  }

  scores <- purrr::map2_dbl(
    alignment_a,
    alignment_b,
    fun_match
  )

  aligners <- rep(":", length(scores))
  aligners <- ifelse(scores == fun_match("d", "d"), "|", aligners)
  aligners <- ifelse(scores == indel, " ", aligners)

  list(
    a_alignment = alignment_a,
    b_alignment = alignment_b,
    scores = scores,
    aligners = aligners
  )
}

#' @export
phone_match_exact <- function(x, y, match = 1, mismatch = -1) {
  if (x == y) match else mismatch
}

#' @export
phone_match_partial <- function(x, y, match = 1, mismatch = -1) {
  consonants <- c(
    "p", "m", "b",
    "t", "d", "n",
    "k", "g", "ŋ",
    "f", "v",
    "θ", "ð",
    "s", "z",
    "ʃ", "ʒ",
    "h",
    "r", "l",
    "tʃ", "dʒ",
    "w",  "j"
  )
  gap <- "."
  c_x <- x %in% consonants
  c_y <- y %in% consonants
  v_x <- ! (x %in% c(consonants, gap, "-"))
  v_y <- ! (y %in% c(consonants, gap, "-"))

  if (x == y) {
    result <- match
  } else if (c_x && c_y) {
    result <- .4 * mismatch
  } else if (v_x && v_y) {
    result <- .6 * mismatch
  } else {
    result <- mismatch
  }
  result
}


