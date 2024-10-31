
#' Align two sequences of phonemes using global alignment
#'
#' The algorithm used here is based on the [Needleman-Wunsch
#' algorithm](https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm).
#'
#' @param a,b two sequences of phonemes to align
#' @param fun_match function to compute the similarity scores for each phoneme
#' @param indel_create penalty for creating a gap
#' @param indel_extend penalty for extending a gap by one step
#' @return a list with the class `phone_alignment` with information about the
#' matching
#' @export
#' @examples
#' # Kiss the sky and kiss this guy
#' a <- c("k", "I", "s", "dh", "4", "s", "k", "@I") |> wiscbet_to_ipa()
#' b <- c("k", "I", "s", "dh", "I", "s", "g", "@I") |> wiscbet_to_ipa()
#' align_phones(a, b)
align_phones <- function(
    a,
    b,
    fun_match = phone_match_exact,
    indel_create = -2,
    indel_extend = -1
) {

  if (! inherits(a, "alignment_utterance")) {
    a <- alignment_utterance(a)
  }
  if (! inherits(b, "alignment_utterance")) {
    b <- alignment_utterance(b)
  }

  # The global alignment problem follows two steps. Filling the matrix with
  # scores and tracing back through it to find the best score. Each of those
  # has their own function.
  grids <- align_grid_setup(
    a,
    b,
    fun_match = fun_match,
    indel_create = indel_create,
    indel_extend = indel_extend
  )

  aligned <- align_grid_trace(
    grid = grids$grid,
    grid_moves = grids$grid_moves,
    fun_match = fun_match
  )

  # put the original strings at the front of the results
  results <- list(a = a, b = b, grids = grids)
  results[names(aligned)] <- aligned

  structure(results, class = c("phone_alignment", "list"))
}



#' @export
set_utterance_labels <- function(x, a = NULL, b = NULL) {
  x$a$label <- a
  x$b$label <- b
  x
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

  if (!is.null(x$a$label)) {
    pad_top <- paste(x$a$label, pad_top, sep = "\n")
  }
  if (!is.null(x$b$label)) {
    pad_bottom <- paste(pad_bottom, x$b$label, sep = "\n")
  }

  paste(pad_top, marks, pad_bottom, sep = "\n")
}

#' @export
as.data.frame.phone_alignment <- function(x, ...) {
  data.frame(
    a = x[["a_alignment"]],
    b = x[["b_alignment"]],
    scores = x[["scores"]]
  )
}


align_grid_setup <- function(
  a,
  b,
  fun_match = phone_match_exact,
  indel_create = -2,
  indel_extend = -1
) {

  as <- a$phones
  bs <- b$phones

  # We keep track of the scores (grid), matching types (grid_edits), and the
  # direction of the winning match (grid_moves)
  grid_moves <- grid_edits <- grid <- matrix(
    nrow = 1 + length(as),
    ncol = 1 + length(bs)
  )

  # The top left corner is an aligned pair of blanks with score 0.
  rownames(grid) <- c("", as)
  colnames(grid) <- c("", bs)

  seq_row_fill <- function(
    matrix,
    prefix = character(0),
    from = 1,
    by = 1
  ) {
    values <- c(prefix, seq(from, length.out = ncol(matrix), by = by))
    values[seq_len(ncol(matrix))]
  }
  seq_col_fill <- function(
    matrix,
    prefix = character(0),
    from = 1,
    by = 1
  ) {
    seq_row_fill(t(matrix), prefix, from, by)
  }
  rep_row_fill <- function(matrix, prefix = character(0), fill) {
    values <- c(prefix, rep(fill, ncol(matrix)))
    values[seq_len(ncol(matrix))]
  }
  rep_col_fill <- function(matrix, prefix = character(0), fill) {
    rep_row_fill(t(matrix), prefix, fill)
  }

  # Walking down the first row or first column will create a gap and then extend
  # it on each step.
  grid_edits[1, ] <- rep_row_fill(grid_edits, c(".match", ".indel_c"), ".indel_e")
  grid_edits[, 1] <- rep_col_fill(grid_edits, c(".match", ".indel_c"), ".indel_e")

  grid_moves[1, ] <- rep_row_fill(grid_moves, ".diag", ".right")
  grid_moves[, 1] <- rep_col_fill(grid_moves, ".diag", ".down")

  grid[1, ] <- seq_row_fill(grid, 0, indel_create, indel_extend)
  grid[, 1] <- seq_col_fill(grid, 0, indel_create, indel_extend)

  lookup <- c(".indel_c" = indel_create, ".indel_e" = indel_extend)

  row_seq <- if (nrow(grid) > 1) seq(2, nrow(grid)) else numeric(0)
  col_seq <- if (ncol(grid) > 1) seq(2, ncol(grid)) else numeric(0)

  for (i in row_seq) {
    for (j in col_seq) {
      # If the square above me is a match, then I am creating a gap.
      # If the square left of me is a match, then I am creating a gap.
      match_u <- grid_edits[i - 1, j    ] == ".match"
      match_l <- grid_edits[i    , j - 1] == ".match"
      indel_d_edit <- ifelse(match_u, ".indel_c", ".indel_e")
      indel_r_edit <- ifelse(match_l, ".indel_c", ".indel_e")
      indel_d_penalty <- lookup[indel_d_edit] |> unname()
      indel_r_penalty <- lookup[indel_r_edit] |> unname()

      a_phone <- rownames(grid)[i]
      b_phone <- colnames(grid)[j]
      score_outcomes <- c(
        grid[i - 1, j - 1] + fun_match(a_phone, b_phone),
        grid[i - 1, j    ] + indel_d_penalty,
        grid[i    , j - 1] + indel_r_penalty
      )
      edit_outcomes <- c(".match", indel_d_edit, indel_r_edit)
      move_outcomes <- c(".diag", ".down", ".right")

      outcome_position <- which.max(score_outcomes)
      grid[i, j] <- score_outcomes[outcome_position]
      grid_edits[i, j] <- edit_outcomes[outcome_position]
      grid_moves[i, j] <- move_outcomes[outcome_position]

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

  is <- i
  js <- j

  # https://en.wikipedia.org/wiki/Needleman%E2%80%93Wunsch_algorithm
  while (i > 1 || j > 1) {
    move <- grid_moves[i, j]
    offsets_i <- c(".diag" = -1, ".down" = -1, ".right" =  0)
    offsets_j <- c(".diag" = -1, ".down" =  0, ".right" = -1)
    new_as <- c(".diag" = grid_a[i], ".down" = grid_a[i], ".right" =  "-")
    new_bs <- c(".diag" = grid_b[j], ".down" =  "-", ".right" = grid_b[j])

    i <- i + offsets_i[move]
    j <- j + offsets_j[move]

    alignment_a <- c(new_as[move], alignment_a)
    alignment_b <- c(new_bs[move], alignment_b)

    is <- c(i, is)
    js <- c(j, js)
  }

  scores <- purrr::map2_dbl(
    alignment_a,
    alignment_b,
    fun_match
  )

  aligners <- rep(":", length(scores))
  aligners <- ifelse(scores == fun_match("d", "d"), "|", aligners)
  aligners <- ifelse(scores == fun_match("d", " "), " ", aligners)

  l <- list(
    a_alignment = alignment_a,
    b_alignment = alignment_b,
    scores = scores,
    aligners = aligners,
    is = is,
    js = js
  )

  lapply(l, unname)
}


align_grid_trace2 <- function(grid, grid_moves, fun_match) {
  grid_a <- rownames(grid)
  grid_b <- colnames(grid)
  alignment_a <- character(0)
  alignment_b <- character(0)
  i <- length(grid_a)
  j <- length(grid_b)

  is <- i
  js <- j

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

    is <- c(i, is)
    js <- c(j, js)
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
    aligners = aligners,
    is = is,
    js = js
  )

}


#' @export
phone_match_exact <- function(x, y, match = 1, mismatch = -1) {
  if (x == y) match else mismatch
}


xxxphone_match_partial <- function(x, y, match = 1, mismatch = -1) {
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
    "p", "m", "b", "f", "v", "θ", "ð", "t", "d", "n", "s", "z",
    "ʃ", "ʒ", "tʃ", "dʒ", "k", "g", "ŋ", "h", "r", "l", "w", "j"
  )
  vowels <- c(
    "i", "ɪ", "ɛ", "æ", "ɜ˞", "ə", "ɚ", "ʌ",
    "ɑ", "u", "ʊ", "ɔ", "aɪ", "aʊ", "e", "o", "ɔɪ"
  )

  gap <- "."
  stopifnot(c(x, y) %in% c(gap, vowels, consonants, "-", " "))

  is_disguise <- list(sort(c(x, y))) %in% disguise_pairs()
  is_friendly <- list(sort(c(x, y))) %in% friendly_pairs()
  both_gap <- all(c(x, y) %in% c(gap, "-", " ", "."))
  c_x <- x %in% consonants
  c_y <- y %in% consonants
  v_x <- x %in% vowels
  v_y <- y %in% vowels

  if (x == y) {
    result <- match
  } else if (is_disguise) {
    result <- match
  } else if (is_friendly) {
    result <- .2 * mismatch
  } else if (c_x && c_y) {
    result <- .4 * mismatch
  } else if (v_x && v_y) {
    result <- .6 * mismatch
  } else if (both_gap) {
    result <- 0
  } else {
    result <- mismatch
  }
  result
}

# treat these pairs as the same sound in disguise but transcribed differently due
# conventions.
disguise_pairs <- function() {
  l <- list(
    c("ʌ", "ə"),
    c("ɚ", "ɝ")
  )
  lapply(l, sort)
}

# similar sounds
friendly_pairs <- function() {
  l <- list(
    # change in voice
    c("p", "b"),
    c("t", "d"),
    c("k", "g"),
    c("f", "v"),
    c("tʃ", "dʒ"),
    c("θ", "ð"),
    c("s", "z"),
    c("ʃ", "ʒ"),
    # change in place
    c("f", "θ"),
    c("s", "ʃ"),
    c("n", "ŋ"),
    # change in manner
    c("p", "f"),
    c("b", "v"),
    # tense-lax pairs
    c("i", "ɪ"),
    c("u", "ʊ"),
    c("e", "ɛ")
  )

  lapply(l, sort)
}




#' @export
str_split_at_hyphens <- function(xs) {
  x1 <- stringr::str_replace_all(xs, "-+", "-")
  unlist(stringr::str_split(x1, "-"))
}

#' @export
clean_old_alignment_result <- function(xs, remove_word_spaces = FALSE) {
  x <- xs |>
    stringr::str_replace_all(" +", "- -") |>
    str_split_at_hyphens() |>
    stringr::str_subset("^$", negate = TRUE)
  if (remove_word_spaces) {
    x[x != " "]
  } else {
    x
  }
}

#' @export
clean_old_alignment_result <- function(xs) {
  xs |>
    stringr::str_replace_all(" +", "- -") |>
    str_split_at_hyphens() |>
    stringr::str_subset("^$", negate = TRUE)
}




#' @export
alignment_utterance <- function(x, label = NA_character_) {
  UseMethod("alignment_utterance")
}

#' @export
alignment_utterance.character <- function(x, label = NA_character_) {
  structure(
    list(phones = x, label = label),
    class = c("alignment_utterance", "list")
  )
}

#' @export
alignment_utterance.alignment_utterance <- function(x) {
  x
}

#' @export
format.alignment_utterance <- function(x) {
  phones <- x$phones |>
    paste0(collapse = " ")

  sprintf("/%s/ (%s)", phones, x$label)
}

#' @export
print.alignment_utterance <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

