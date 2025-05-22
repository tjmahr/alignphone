
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alignphone

<!-- badges: start -->
<!-- badges: end -->

The goal of alignphone is to compute phoneme-by-phoneme alignments of
two strings of words.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/alignphone")
```

## Example

We want to align the phonemes from the sentence “the little boy went
home” with the sentence “the boy went home”.

First, we create `alignment_utterance()` for each sentence. These
provide an orthographic version of the sentence and the constituent
phonemes. Our lab used/uses an ASCII substitute for IPA, so I am going
to convert the provide the phonemes in “wiscbet” and convert them to IPA
before creating the alignment utterance.

``` r
library(alignphone)

a <- wiscbet_to_ipa(
  "dh", "4", " ", 
  "l", "I", ".", "t", "4", "l", " ", 
  "b", "cI", " ", 
  "r", "ae", "n", " ",  
  "h", "oU", "m"
) |> 
  alignment_utterance("the little boy went home")
a
#> /ð ə   l ɪ . t ə l   b ɔɪ   r æ n   h o m/ (the little boy went home)

b <- wiscbet_to_ipa(
  "dh", "4", " ", 
  "b", "cI", " ", 
  "w", "E", "n", "t", " ", 
  "h", "oU", "m"
)|> 
  alignment_utterance("the boy went home")
b
#> /ð ə   b ɔɪ   w ɛ n t   h o m/ (the boy went home)
```

Now, we can align the two utterances.

``` r
ab1 <- align_phones(a, b)
ab1
#> the little boy went home
#> ð ə   l ɪ . t ə l   b ɔɪ   r æ n -   h o m
#> | | |               | |  |     |   | | | |
#> ð ə   - - - - - - - b ɔɪ   w ɛ n t   h o m
#> the boy went home
```

The goal of the alignment algorithm is to create the best-scoring
pairing of phonemes from each utterance. Matching phonemes get positive
points, and mismatching phonemes get negative points. Here is what we
see in the above example:

- The initial /ð ə/ phonemes are exact matches and pair off perfectly.
  These full-score pairings are indicated with `|` character.

- The space `` after the word “boy” in each sentence is also aligned.

- The word “little” is completely missing in one of the sentences but
  the word “boy” is in both sentences. So, the alignment inserts gap
  characters (`-`) as needed until the /b ɔɪ/ characters are aligned.

- “ran” and “went” are aligned so that the shared /n/ is fully scored.

- The phonemes /h o m/ are also aligned.

By default, the alignment only rewards exact matches (`|`), and gap
insertion is penalized. The `as.data.frame()` method for the alignment
shows the alignment with scores.

``` r
as.data.frame(ab1)
#>     a  b scores
#> 1   ð  ð      1
#> 2   ə  ə      1
#> 3             1
#> 4   l  -     -1
#> 5   ɪ  -     -1
#> 6   .  -     -1
#> 7   t  -     -1
#> 8   ə  -     -1
#> 9   l  -     -1
#> 10     -     -1
#> 11  b  b      1
#> 12 ɔɪ ɔɪ      1
#> 13            1
#> 14  r  w     -1
#> 15  æ  ɛ     -1
#> 16  n  n      1
#> 17  -  t     -1
#> 18            1
#> 19  h  h      1
#> 20  o  o      1
#> 21  m  m      1
```

A custom comparison function can be used for alignment. Note that the
pairing of the word boundary character (``) or the syllable boundary
(`.`) with a gap (`-`) is still penalized. This package provides
`phone_match_partial()` which assigns partial credit based similar
phonetic features, such as `w` and `r`. Partial matches appear in the
alignment as `:`.

``` r
ab2 <- align_phones(a, b, fun_match = phone_match_partial)
ab2
#> the little boy went home
#> ð ə   l ɪ . t ə l   b ɔɪ   r æ n -   h o m
#> | | |     :       : | |  | : : |   | | | |
#> ð ə   - - - - - - - b ɔɪ   w ɛ n t   h o m
#> the boy went home

as.data.frame(ab2)
#>     a  b scores
#> 1   ð  ð    1.0
#> 2   ə  ə    1.0
#> 3           1.0
#> 4   l  -   -1.0
#> 5   ɪ  -   -1.0
#> 6   .  -    0.0
#> 7   t  -   -1.0
#> 8   ə  -   -1.0
#> 9   l  -   -1.0
#> 10     -    0.0
#> 11  b  b    1.0
#> 12 ɔɪ ɔɪ    1.0
#> 13          1.0
#> 14  r  w   -0.4
#> 15  æ  ɛ   -0.6
#> 16  n  n    1.0
#> 17  -  t   -1.0
#> 18          1.0
#> 19  h  h    1.0
#> 20  o  o    1.0
#> 21  m  m    1.0
```

Here /r/ and /w/ as well as the two aligned mismatching vowels are given
partial credit.

### indel penalties

Above, we saw that changing the phoneme-similarity scoring system
yielded a different alignment. The other contributor for alignment
scoring are “indel” penalties. These are penalties for creating a gap
(`indel_create` with a default penalty of -2) and extending a gap
(`indel_extend` with a default of -1).

<!--  Our lab's legacy program for phoneme -->
<!-- alignments---its C++ source code is impenetrable to me, hence this -->
<!-- package---provides the following alignment: -->
<!-- ``` -->
<!-- b-eI-b-i s-------@-k -->
<!--        ||        | | -->
<!-- w------i r-E-s-t-@-k -->
<!-- ``` -->
<!-- I can reproduce this strange alignment by lowering the indel penalties: -->

Consider another example. A child speaker said “baby sock” and a
listener transcribed it as “we restock”. With the default scoring rules
(`phone_match_exact`), we get the following alignment:

``` r
a <- wiscbet_to_ipa("b", "eI", "b", "i", "s", "@", "k") |> 
  alignment_utterance("baby sock")
b <- wiscbet_to_ipa("w", "i", "r", "I", "s", "t", "@", "k") |> 
  alignment_utterance("we restock")
align_phones(a, b, phone_match_exact)
#> baby sock
#> b e b i s - ɑ k
#>         |   | |
#> w i r ɪ s t ɑ k
#> we restock
```

Which only inserts a single gap for `t`. If we lower the penalty for
creating a gap, we see that it tries to align the two /i/ vowels.

``` r
align_phones(a, b, phone_match_exact, indel_create = -.5)
#> baby sock
#> b e b i - - s - ɑ k
#>       |     |   | |
#> - w - i r ɪ s t ɑ k
#> we restock
```

Things become pathological when we remove the penalty on gap extension.

``` r
align_phones(a, b, phone_match_exact, indel_extend = 0)
#> baby sock
#> - - - - - - b e b i s ɑ k
#>                       | |
#> w i r ɪ s t - - - - - ɑ k
#> we restock
```

To get the most phonetically plausible alignment, we can assign partial
credit.

``` r
align_phones(a, b, phone_match_partial)
#> baby sock
#> b e b i s - ɑ k
#> : : : : |   | |
#> w i r ɪ s t ɑ k
#> we restock
```

I am not sure what to do with boundaries. We can word and syllable
boundaries so that alignments will get credit for following matching the
prosodic structure of the words, but for this example, this adjustment
doesn’t matter.

``` r
a <- wiscbet_to_ipa("b", "eI", ".", "b", "i", " ", "s", "@", "k") |> 
  alignment_utterance("baby sock")
b <- wiscbet_to_ipa("w", "i", " ", "r", "I", ".", "s", "t", "@", "k") |> 
  alignment_utterance("we restock")
align_phones(a, b, phone_match_exact)
#> baby sock
#> b e . b i   s - ɑ k
#>             |   | |
#> w i   r ɪ . s t ɑ k
#> we restock
align_phones(a, b, phone_match_partial)
#> baby sock
#> b e . b i   s - ɑ k
#> : : : : : : |   | |
#> w i   r ɪ . s t ɑ k
#> we restock
```

## More tests

### pretty-buddy

This is an important benchmark. Given “pretty” for *buddy*, what sound
should align at the /b/“? The default approach pairs \[b\] and \[r\]. A
partial credit scheme that assigns -.6 to mismatched vowels and -.4 to
mismatched consonants will pair \[b\] and \[r\]. Our partial credit
function assigns -.2 to mismatches that are very similar. In this case,
it aligns \[b\] and \[p\].

``` r
buddy <- str_split_at_hyphens("b-^-d-i") |> 
  wiscbet_to_ipa() |> 
  alignment_utterance("buddy")
pretty <- str_split_at_hyphens("p-r-I-t-i") |> 
  wiscbet_to_ipa() |> 
  alignment_utterance("pretty")

align_phones(buddy, pretty)
#> buddy
#> - b ʌ d i
#>         |
#> p r ɪ t i
#> pretty

align_phones(buddy, pretty, phone_match_partial)
#> buddy
#> b - ʌ d i
#> :   : : |
#> p r ɪ t i
#> pretty
align_phones(buddy, pretty, phone_match_partial) |> as.data.frame()
#>   a b scores
#> 1 b p   -0.2
#> 2 - r   -1.0
#> 3 ʌ ɪ   -0.6
#> 4 d t   -0.2
#> 5 i i    1.0
```

### Deleted words

This is okay.

``` r
point_to_teddy <- clean_old_alignment_result("p-cI-n-t t-u t-E-d-i") |> 
  wiscbet_to_ipa() |> 
  alignment_utterance("point to teddy")
point_teddy <- clean_old_alignment_result("p-cI-n-t ----t-E-d-i") |> 
  wiscbet_to_ipa() |> 
  alignment_utterance("point teddy")

align_phones(point_to_teddy, point_teddy, phone_match_partial)
#> point to teddy
#> p ɔɪ n t   t u   t ɛ d i
#> | |  | | |     : | | | |
#> p ɔɪ n t   - - - t ɛ d i
#> point teddy
```

This treats *hotdogs* and *hot dogs* as a mismatch on the space. It
shouldn’t.

``` r
a <- "dh-oU-z i-t dh-oU-z h-@-t-d-c-g-z s-u-n" |> 
  clean_old_alignment_result() |> 
  wiscbet_to_ipa() |> 
  alignment_utterance("those eat those hotdogs soon")
a
#> /ð o z   i t   ð o z   h ɑ t d ɔ g z   s u n/ (those eat those hotdogs soon)

b <- "--------------------h-@-t  d-c-g-z------" |> 
  clean_old_alignment_result() |> 
  wiscbet_to_ipa() |> 
  alignment_utterance("hot dogs")
b
#> /h ɑ t   d ɔ g z/ (hot dogs)

r1 <- align_phones(a, b, phone_match_partial)
r1
#> those eat those hotdogs soon
#> ð o z   i t   ð o z   h ɑ t - d ɔ g z   s u n
#>       :     :       : | | | : | | | | :      
#> - - - - - - - - - - - h ɑ t   d ɔ g z - - - -
#> hot dogs
```

This one works when there are word gap characters.

``` r
he_wants_somebody_to_push_him <- clean_old_alignment_result(
  "h-i w-^-n-t-s s-^-m-b-^-d-i t-u p-U-sh h-I-m"
) |> 
  wiscbet_to_ipa() |> 
  alignment_utterance("he wants somebody to push him")  

he_wants_to_push_him <- clean_old_alignment_result(
  "h-i w-^-n-t-s --------------t-u p-U-sh h-I-m"
) |> 
  wiscbet_to_ipa() |> 
  alignment_utterance("he wants to push him")  

r1 <- align_phones(
  he_wants_somebody_to_push_him, 
  he_wants_to_push_him, 
  phone_match_partial
)

r1
#> he wants somebody to push him
#> h i   w ʌ n t s   s ʌ m b ʌ d i   t u   p ʊ ʃ   h ɪ m
#> | | | | | | | | |               : | | | | | | | | | |
#> h i   w ʌ n t s   - - - - - - - - t u   p ʊ ʃ   h ɪ m
#> he wants to push him
```

But when the gaps are removed, it does not work.

``` r
remove_gaps <- function(x) {
  gaps <- x$phones %in% c(" ", ".")
  x$phones <- x$phones[!gaps]
  x
}


r1 <- align_phones(
  he_wants_somebody_to_push_him |> remove_gaps(), 
  he_wants_to_push_him |> remove_gaps(), 
  phone_match_partial
)
r1
#> he wants somebody to push him
#> h i w ʌ n t s s ʌ m b ʌ d i t u p ʊ ʃ h ɪ m
#> | | | | | |   |             | | | | | | | |
#> h i w ʌ n t - s - - - - - - t u p ʊ ʃ h ɪ m
#> he wants to push him
```

### the swift benchmark

I really want this one to match `b`-`v` and `li`-`lI`, but I have to
ignore word boundaries entirely. I need to figure out how to favor the
kind of alignment.

``` r
x <- wiscbet_to_ipa(
    "l", "@", "ng", 
    "l", "I", "s", "t", 
    "^", "v", 
    "E", "k", "s", 
    "l", "^", "v", "4^", "z"
  )

y <- wiscbet_to_ipa(
    "l", "oU", "n", "l", "i", 
    "s", "t", "@", "r", "b", "^", "k", "s", 
    "l", "^", "v", "4^", "z"
  )


z <- align_phones(x, y, phone_match_partial) |> 
  set_utterance_labels(
    "long list of ex-lovers",
    "lonely starbucks lovers"
  ) |> 
  print() 
#> long list of ex-lovers
#> l ɑ ŋ l ɪ s t ʌ - v ɛ k s l ʌ v ɚ z
#> | : : | : | | :   : : | | | | | | |
#> l o n l i s t ɑ r b ʌ k s l ʌ v ɚ z
#> lonely starbucks lovers


x2 <- wiscbet_to_ipa(
    "l", "@", "ng", " ",
    "l", "I", "s", "t", " ",
    "^", "v", " ",
    "E", "k", "s", " ", 
    "l", "^", ".", "v", "4^", "z"
  )

y2 <- wiscbet_to_ipa(
    "l", "oU", "n", ".", "l", "i", " ",
    "s", "t", "@", "r", ".", "b", "^", "k", "s", " ",
    "l", "^", ".", "v", "4^", "z"
  )

z <- align_phones(x2, y2, phone_match_partial) |> 
  set_utterance_labels(
    "long list of ex-lovers",
    "lonely starbucks lovers"
  ) |> 
  print() 
#> long list of ex-lovers
#> l ɑ ŋ   l ɪ - s t   ʌ v   ɛ k s   l ʌ . v ɚ z
#> | : : : | : : | |         : | | | | | | | | |
#> l o n . l i   s t ɑ r . b ʌ k s   l ʌ . v ɚ z
#> lonely starbucks lovers
```
