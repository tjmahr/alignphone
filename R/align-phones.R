
#' Align two sequences of phonemes using global alignment
#'
#' @param a,b two sequences of phonemes to align
#' @param fun_match function to compute the similarity scores for each phoneme
#' @param indel_create score (penalty) for creating a gap
#' @param indel_extend score (penalty) for extending a gap by one step
#' @return a list with information about the matching
#' @export
align_phones <- function(a, b, fun_match = phone_match_exact, indel_create = -2, indel_extend = -1) {
  grids <- align_grid_setup(
    a,
    b,
    fun_match = fun_match,
    indel_create = indel_create,
    indel_extend = indel_extend
  )

  aligned <- align_grid_trace(
    grids$grid,
    grids$grid_moves,
    fun_match = fun_match
  )

  # put the original strings at the front of the results
  results <- list(a = a, b = b)
  results[names(aligned)] <- aligned

  structure(results, class = c("phone_alignment", "list"))
}


#' @export
print.phone_alignment <- function(x, ...) {
  writeLines(format(x))
  invisible(x)
}

#' @export
format.phone_alignment <- function(x, ...) {
  matches <- x$scores
  lengths1 <- nchar(x$a_alignment)
  lengths2 <- nchar(x$b_alignment)
  lengths <- pmax(lengths1, lengths2)

  pad_top <- x$a_alignment |>
    stringr::str_pad(lengths, side = "right") |>
    paste0(collapse = " ")

  pad_bottom <- x$b_alignment |>
    stringr::str_pad(lengths, side = "right") |>
    paste0(collapse = " ")

  marks <- x$aligners |>
    stringr::str_pad(lengths, side = "right") |>
    paste0(collapse = " ")

  paste(pad_top, marks, pad_bottom, sep = "\n")
}




align_grid_setup <- function(a, b, fun_match = phone_match_exact,
                             indel_create = -1, indel_extend = -1) {
  grid_moves <- grid_edits <- grid <- matrix(
    nrow = 1 + length(a),
    ncol = 1 + length(b)
  )

  # set up initial insertions
  grid[1, ] <- c(
    0,
    seq(indel_create, length.out = ncol(grid) - 1, by = indel_extend)
  )
  grid[, 1] <- c(
    0,
    seq(indel_create, length.out = nrow(grid) - 1, by = indel_extend)
  )
  rownames(grid) <- c("", a)
  colnames(grid) <- c("", b)

  grid_edits[1, ] <- c(".match", rep(".indel", ncol(grid) - 1))
  grid_edits[, 1] <- c(".match", rep(".indel", nrow(grid) - 1))

  grid_moves[1, ] <- c(".diag", rep(".right", ncol(grid) - 1))
  grid_moves[, 1] <- c(".diag", rep(".down", nrow(grid) - 1))

  # value in each cell is either a match from the diagonal or an indel
  for (i in seq(2, nrow(grid))) {
    for (j in seq(2, ncol(grid))) {
      indel_penalty_down <- ifelse(
        grid_edits[i - 1, j    ] == ".match",
        indel_create,
        indel_extend
      )
      indel_penalty_right <- ifelse(
        grid_edits[i    , j - 1] == ".match",
        indel_create,
        indel_extend
      )

      outcomes <- c(
        .match  = grid[i - 1, j - 1] + fun_match(c("", a)[i], c("", b)[j]),
        .delete = grid[i - 1, j    ] + indel_penalty_down,
        .insert = grid[i    , j - 1] + indel_penalty_right
      )

      grid[i, j] <- max(outcomes)

      grid_edits[i, j] <- ifelse(
        names(which.max(outcomes)) == ".match",
        ".match",
        ".indel"
      )

      grid_moves[i, j] <- c(".diag", ".down", ".right")[which.max(outcomes)]
    }
  }

  list(
    grid = grid,
    grid_edits = grid_edits,
    grid_moves = grid_moves
  )
}

align_grid_trace <- function(grid, grid_moves, fun_match) {
  grid_a <- rownames(grid)
  grid_b <- colnames(grid)
  alignment_a <- character(0)
  alignment_b <- character(0)
  i <- length(grid_a)
  j <- length(grid_b)

  # https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
  while (i > 1 || j > 1) {
    diagonal <- grid_moves[i, j] == ".diag"
    down <- grid_moves[i, j] == ".down"

    if (diagonal) {
      alignment_a <- c(grid_a[i], alignment_a)
      alignment_b <- c(grid_b[j], alignment_b)
      i <- i - 1
      j <- j - 1
    } else if (down) {
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
  aligners <- ifelse(scores == fun_match("d", " "), " ", aligners)

  list(
    a_alignment = alignment_a,
    b_alignment = alignment_b,
    scores = scores,
    aligners = aligners
  )

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
    not_top_row <- i > 1
    not_left_row <- j > 1
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

#' @export
phone_match_partial <- function(x, y, match = 1, mismatch = -1) {
  consonants <- c(
    "p", "m", "b",
    "f", "v",
    "th", "dh",
    "t", "d", "n",
    "s", "z",
    "sh", "zh",
    "tsh", "dzh",
    "k", "g", "ng",
    "h",
    "r", "l",
    "w",  "j"
  )

  # personally i think this vowel inventory is overspecified and some of the
  # conventions are wrong, but these are the symbols used in our pronunciation
  # dictionary
  vowels <- c(
    # i, ɪ, e, ɛ, æ
    "i", "I", "e", "E", "ae",
    # ɜ, ɜ˞, ə, ɚ, ʌ
    "3", "3^", "4", "4^", "^",
    # a, ɑ, ɒ
    "a", "@", "D",
    # u, ʊ, o, ɔ
    "u", "U", "o", "c",
    # ɑɪ, ɑʊ, eɪ, oʊ, ɔɪ
    "@I", "@U", "eI", "oU", "cI"
  )

  gap <- "."
  stopifnot(c(x, y) %in% c(gap, vowels, consonants, "-", " "))

  c_x <- x %in% consonants
  c_y <- y %in% consonants
  v_x <- x %in% vowels
  v_y <- y %in% vowels

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


#' @export
phone_match_aline <- function(x, y, match = 1, mismatch = -1) {
  consonants <- c(
    "p" = "p",
    "m" = "m",
    "b" = "b",
    "f" = "f",
    "v" = "v",
    "th" = intToUtf8(952),
    "dh" = intToUtf8(240),
    "t" = "t",
    "d" = "d",
    "n" = "n",
    "s" = "s",
    "z" = "z",
    "sh" = intToUtf8(643),
    "zh" = intToUtf8(658),
    "tsh" = intToUtf8(679),
    "dzh" = intToUtf8(676),
    "k" = "k",
    "g" = "g",
    "ng" = intToUtf8(331),
    "h" = "h",
    "r" = "r",
    "l" = "l",
    "w" = "w",
    "j" = "j"
  )

  vowels <- c(
    # i, ɪ, e, ɛ, æ
    "i" = "i",
    "I" = intToUtf8(618),
    "e" = "e",
    "E" = intToUtf8(603),
    "ae" = intToUtf8(230),
    # ɜ, ɜ˞, ə, ɚ, ʌ
    "3" = intToUtf8(604),
    # "3^" = intToUtf8(605),
    "3^" = intToUtf8(605),
    "4" = intToUtf8(601),
    "4^" = intToUtf8(602),
    "^" = intToUtf8(652),
    # a, ɑ, ɒ
    "a" = "a",
    "@" = intToUtf8(593),
    "D" = intToUtf8(594),
    # u, ʊ, o, ɔ
    "u" = "u",
    "U" = intToUtf8(650),
    "o" = "o",
    "c" = intToUtf8(596),
    # ɑɪ, ɑʊ, eɪ, oʊ, ɔɪ
    "@I" = intToUtf8(c(593, 618)),
    "@U" = intToUtf8(c(593, 650)),
    "eI" = intToUtf8(c(101, 618)),
    "oU" = intToUtf8(c(111, 650)),
    "cI" = intToUtf8(c(596, 618))
  )

  sounds <- c(vowels, consonants)

  m1 <- c(intToUtf8(602), intToUtf8(605))
  m2 <- c("eCX", "eCX")

  gap <- "."
  stopifnot(c(x, y) %in% c(gap, names(sounds), "-", " "))

  c_x <- x %in% names(sounds)
  c_y <- y %in% names(sounds)



  # if (x == y) {
  #   result <- match
  # } else if (all(c(x, y) %in% c("-", " "))) {
  #   result <- 0
  # } else if (c_x && c_y) {
  #   x <- sounds[x]
  #   y <- sounds[y]
  #   dist <- alineR::aline(x, y, sim = FALSE, m1 = m1, m2 = m2)
  #   result <- (1 - dist) * match
  # } else {
  #   result <- mismatch
  # }
  # result

  if (c_x && c_y) {
    x <- sounds[x]
    y <- sounds[y]
    dist <- alineR::aline(x, y, sim = FALSE, m1 = m1, m2 = m2)
    result <- (1 - dist) * match

    if (x == y) {
      result <- match
    }

  } else if (all(c(x, y) %in% c("-", " "))) {
      result <- 0
  } else {
    result <- mismatch
  }
  result
}

#' @export
str_split_at_hyphens <- function(xs) {
  x1 <- stringr::str_replace_all(xs, "-+", "-")
  unlist(stringr::str_split(x1, "-"))
}

#' @export
clean_old_alignment_result <- function(xs) {
  xs %>%
    stringr::str_replace_all(" +", "- -") %>%
    str_split_at_hyphens() %>%
    stringr::str_subset("^$", negate = TRUE)
}

