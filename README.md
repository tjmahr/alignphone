
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
`:`. The `as.data.frame()` method for the alignment shows the alignment
with scores.

``` r
ab2 <- align_phones(a, b, fun_match = phone_match_partial)
ab2
#> dh 4 . l I t l . b cI . r ae n - . h oU m
#> |  |           | | |  | : :  |   | | |  |
#> dh 4 - - - - - . b cI . w E  n t . h oU m

as.data.frame(ab2)
#>     a  b scores
#> 1  dh dh    1.0
#> 2   4  4    1.0
#> 3   .  -   -1.0
#> 4   l  -   -1.0
#> 5   I  -   -1.0
#> 6   t  -   -1.0
#> 7   l  -   -1.0
#> 8   .  .    1.0
#> 9   b  b    1.0
#> 10 cI cI    1.0
#> 11  .  .    1.0
#> 12  r  w   -0.4
#> 13 ae  E   -0.6
#> 14  n  n    1.0
#> 15  -  t   -1.0
#> 16  .  .    1.0
#> 17  h  h    1.0
#> 18 oU oU    1.0
#> 19  m  m    1.0
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
as.data.frame(ab2)
#>    a b scores
#> 1  b w   -0.4
#> 2 eI i   -0.6
#> 3  b r   -0.4
#> 4  i E   -0.6
#> 5  s s    1.0
#> 6  - t   -1.0
#> 7  @ @    1.0
#> 8  k k    1.0
```

Here /s/ is matched with \[s\] and no sounds besides /t/ were deleted.

## More tests

### pretty-buddy

This is an important benchmark. Given “pretty” for *buddy*, what sound
should align at the /b/“? The default approach pairs \[b\] and \[r\]. A
partial credit scheme that assigns -.6 to mismatched vowels and -.4 to
mismatched consonants will pair \[b\] and \[r\]. Our partial credit
function assigns -.2 to mismatches that are very similar. In this case,
it aligns \[b\] and \[p\].

``` r
buddy <- str_split_at_hyphens("b-^-d-i")
pretty <- str_split_at_hyphens("p-r-I-t-i")

align_phones(buddy, pretty) |> 
  print() |> 
  as.data.frame()
#> - b ^ d i
#>         |
#> p r I t i
#>   a b scores
#> 1 - p     -1
#> 2 b r     -1
#> 3 ^ I     -1
#> 4 d t     -1
#> 5 i i      1

align_phones(buddy, pretty, phone_match_partial) |> 
  print() |> 
  as.data.frame()
#> b - ^ d i
#> :   : : |
#> p r I t i
#>   a b scores
#> 1 b p   -0.2
#> 2 - r   -1.0
#> 3 ^ I   -0.6
#> 4 d t   -0.2
#> 5 i i    1.0
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
