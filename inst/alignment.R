setup_grid <- function(a, b, fun_match = check, indel = -1) {
  grid <- matrix(
    nrow = 1 + length(a),
    ncol = 1 + length(b)
  )

  # setup insertions
  grid[1, ] <- seq(0, length.out = ncol(grid), by = -1)
  grid[, 1] <- seq(0, length.out = nrow(grid), by = -1)
  rownames(grid) <- c("", a)
  colnames(grid) <- c("", b)

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



x <- c(
  # the bird landed behind the shaggy dog
  "dh-4 b-3^-d l-ae-n-d-I-d b-i-h-@I-n-d dh-4 sh-ae-g-i d-c-g")
y <- c(
  # the bird landed behind shaky dog
  "dh-4 b-3^-d l-ae-n-d-I-d b-i-h-@I-n-d sh-e-k-i d-c-g")

to_ipa <- function(xs) {
  xs %>%
    stringr::str_replace_all("dh", "ð") %>%
    stringr::str_replace_all("4", "ə") %>%
    stringr::str_replace_all("3\\^", "ɜr") %>%
    stringr::str_replace_all("ae", "æ") %>%
    stringr::str_replace_all("I", "ɪ") %>%
    stringr::str_replace_all("c", "ɔ")
}


a <- c("ð", "ə", ".", "l", "ɪ", "t", "l", ".", "b", "ɔɪ", ".", "r", "æ", "n", ".",  "h", "oʊ", "m")
b <- c("ð", "ə", ".", "b", "ɔɪ", ".", "w", "E", "n", "t", ".", "h", "oʊ", "m")


align_sentences <- function(a, b, fun_match = check, indel = -1) {
  grid <- setup_grid(a, b, fun_match = fun_match, indel = indel)
  aligned <- walk_grid(grid, fun_match = fun_match, indel = indel)
  results <- list(a = a, b = b)

  results[names(aligned)] <- aligned
  results
}

walk_grid <- function(grid, fun_match = check, indel = -1) {
  grid_a <- rownames(grid)
  grid_b <- colnames(grid)

  alignment_a <- character(0)
  alignment_b <- character(0)
  i <- length(grid_a)
  j <- length(grid_b)

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

  a_alignment <- alignment_a
  b_alignment <- alignment_b

  scores <- purrr::map2_dbl(
    a_alignment,
    b_alignment,
    fun_match
  )

  aligners <- rep(":", length(scores))
  aligners <- ifelse(scores == fun_match("d", "d"), "|", aligners)
  aligners <- ifelse(scores == indel, " ", aligners)

  list(
    a_alignment = a_alignment,
    b_alignment = b_alignment,
    scores = scores,
    aligners = aligners
  )
}



library(magrittr)

alignment <- align_sentences(a, b, check2)
print_alignment(alignment)


x <- c(
  # the bird landed behind the shaggy dog
  "dh-4 b-3^-d l-ae-n-d-I-d b-i-h-@I-n-d dh-4 sh-ae-g-i d-c-g")
y <- c(
  # the bird landed behind shaky dog
  "dh-4 b-3^-d l-ae-n-d-I-d b-i-h-@I-n-d sh-e-k-i d-c-g")

a <- x %>%
  stringr::str_replace_all(" ", "-.-") %>%
  stringr::str_split("-") %>%
  unlist()
b <- y %>%
  stringr::str_replace_all(" ", "-.-") %>%
  stringr::str_split("-") %>%
  unlist()


grid <- setup_grid(a, b, check2)
grid
alignment <- walk_grid(grid, fun_match = check2)
alignment
print_alignment(alignment)
fix_ipa <- function(x) {
  x %>%
    stringr::str_replace_all("3\\^", "3r") %>%
    stringr::str_replace_all("@I", "aI") %>%
    stringr::str_replace_all("@U", "aU") %>%
    stringr::str_replace_all("4\\^", "4\\r") %>%
    stringr::str_replace_all("@", "a") %>%
    stringr::str_replace_all(" \\^", " A") %>%
    stringr::str_replace_all("-\\^", "-A")
}

tests <- yaml::read_yaml("align.yaml")

test <- tests[[1]]
for (test in tests) {
  clean_input <- . %>%
    fix_ipa() %>%
    to_ipa() %>%
    stringr::str_replace_all("-+", "-") %>%
    stringr::str_replace_all(" -", " ") %>%
    stringr::str_replace_all("- ", " ")

  str_tokenize <- . %>%
    stringr::str_replace_all(" ", "-.-") %>%
    stringr::str_split("-") %>%
    unlist()

  x <- test$char_a %>% clean_input() %>% str_tokenize()
  y <- test$char_b %>% clean_input() %>% str_tokenize()

  cat(test$gloss_a, "\n")
  cat(test$char_a, "\n")
  grid <- setup_grid(x, y, check2)
  alignment <- walk_grid(grid, check2)
  print_alignment(alignment)
  cat(test$char_b, "\n")
  cat(test$gloss_b, "\n")

  cat("\n")

}
