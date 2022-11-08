
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alignphone

<!-- badges: start -->
<!-- badges: end -->

The goal of alignphone is to …

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/alignphone")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(alignphone)

# the little boy ran home
a <- c(
  "dh", "4", ".", 
  "l", "I", "t", "l", ".", 
  "b", "cI", ".", 
  "r", "ae", "n", ".",  
  "h", "oU", "m"
)

# the boy went home
b <- c(
  "dh", "4", ".", 
  "b", "cI", ".", 
  "w", "E", "n", "t", ".", 
  "h", "oU", "m"
)
```

By default, the alignment only rewards exact matches. These appear in
the alignment as `|`.

``` r
ab1 <- align_phones(a, b)
ab1
#> dh 4 . l I t l . b cI . r ae n - . h oU m
#> |  |           | | |  |      |   | | |  |
#> dh 4 - - - - - . b cI . w E  n t . h oU m

ab1$scores
#>  [1]  1  1 -1 -1 -1 -1 -1  1  1  1  1 -1 -1  1 -1  1  1  1  1
```

A custom comparison function can be used for alignment. This package
provides `phone_match_partial()` which assigns partial credit based
similar phonetic features. Partial matches appear in the alignment as
`:`.

``` r
ab2 <- align_phones(a, b, fun_match = phone_match_partial)
ab2
#> dh 4 . l I t l . b cI . r ae n - . h oU m
#> |  |           | | |  | : :  |   | | |  |
#> dh 4 - - - - - . b cI . w E  n t . h oU m

ab2$scores
#>  [1]  1.0  1.0 -1.0 -1.0 -1.0 -1.0 -1.0  1.0  1.0  1.0  1.0 -0.4 -0.6  1.0 -1.0
#> [16]  1.0  1.0  1.0  1.0
```

Our current (non-R) alignment program aligns “baby sock” (child) and “we
restock” (listener) as:

    b-eI-b-i s-------@-k
           ||        | |
    w------i r-E-s-t-@-k

This says that the listener heard deletions of “eI” and “b” and a
substitution of \[r\] for /s/.

But the partial-credit implementation here yields a better alignment.

``` r
a <- c("b", "eI", "b", "i", "s", "@", "k")
b <- c("w", "i", "r", "E", "s", "t", "@", "k")
ab2 <- align_phones(a, b, phone_match_partial)
ab2
#> b eI b i s - @ k
#> : :  : : |   | |
#> w i  r E s t @ k
ab2$scores
#> [1] -0.4 -0.6 -0.4 -0.6  1.0 -1.0  1.0  1.0
```

Here /s/ is matched with \[s\] and no sounds were deleted.

## More tests

``` r
buddy <- str_split_at_hyphens("b-^-d-i")
pretty <- str_split_at_hyphens("p-r-I-t-i")

align_phones(buddy, pretty)
#> - b ^ d i
#>         |
#> p r I t i
align_phones(buddy, pretty, phone_match_partial)
#> - b ^ d i
#>   : : : |
#> p r I t i
```

``` r
point_to_teddy <- clean_old_alignment_result("p-cI-n-t t-u t-E-d-i")
point_teddy <- clean_old_alignment_result("p-cI-n-t ----t-E-d-i")

align_phones(point_to_teddy, point_teddy, phone_match_partial)
#> p cI n t   t u   t E d i
#> | |  | | |       | | | |
#> p cI n t   - - - t E d i
```

``` r
those_eat_those_hotdogs_soon <- clean_old_alignment_result(
  "dh-oU-z i-t dh-oU-z h-@-t--d-c-g-z  s-u-n"
) 
hot_dogs <- clean_old_alignment_result(
  "--------------------h-@-t  d-c-g-z------"
)

r1 <- align_phones(those_eat_those_hotdogs_soon, hot_dogs, phone_match_partial)
r1
#> dh oU z   i t   dh oU z   h @ t - d c g z   s u n
#>                           | | |   | | | |        
#> -  -  - - - - - -  -  - - h @ t   d c g z - - - -
```
