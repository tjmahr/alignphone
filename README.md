
<!-- README.md is generated from README.Rmd. Please edit that file -->
alignphone
==========

<!-- badges: start -->
<!-- badges: end -->
The goal of alignphone is to ...

Installation
------------

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/alignphone")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(alignphone)

# the little boy ran home
a <- c(
  "ð", "ə", ".", 
  "l", "ɪ", "t", "l", ".", 
  "b", "ɔɪ", ".", 
  "r", "æ", "n", ".",  
  "h", "oʊ", "m"
)

# the boy went home
b <- c(
  "ð", "ə", ".", 
  "b", "ɔɪ", ".", 
  "w", "E", "n", "t", ".", 
  "h", "oʊ", "m"
)
```

By default, the alignment only rewards exact matches. These appear in the alignment as `|`.

``` r
ab1 <- align_phones(a, b)
ab1
#> ð ə . l ɪ t l . b ɔɪ . r æ n - . h oʊ m
#> | |           | | |  |     |   | | |  | 
#> ð ə - - - - - . b ɔɪ . w E n t . h oʊ m

ab1$scores
#>  [1]  1  1 -1 -1 -1 -1 -1  1  1  1  1 -1 -1  1 -1  1  1  1  1
```

A custom comparison function can be used for alignment. This package provides `phone_match_partial()` which assigns partial credit based similar phonetic features. Partial matches appear in the alignment as `:`.

``` r
ab2 <- align_phones(a, b, fun_match = phone_match_partial)
ab2
#> ð ə . l ɪ t l . b ɔɪ . r æ n - . h oʊ m
#> | |           | | |  | : : |   | | |  | 
#> ð ə - - - - - . b ɔɪ . w E n t . h oʊ m

ab2$scores
#>  [1]  1.0  1.0 -1.0 -1.0 -1.0 -1.0 -1.0  1.0  1.0  1.0  1.0 -0.4 -0.6  1.0
#> [15] -1.0  1.0  1.0  1.0  1.0
```
