
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alignphone

The goal of alignphone is to …

## the problem

We get two sequences of words segmented out into phonemes. The question
is how to align these two sequences.

``` r
x <- c(
  # the bird landed behind the shaggy dog
  "dh-4 b-3^-d l-ae-n-d-I-d b-i-h-@I-n-d dh-4 sh-ae-g-i d-c-g")
y <- c(
  # the bird landed behind shaky dog
  "dh-4 b-3^-d l-ae-n-d-I-d b-i-h-@I-n-d sh-e-k-i d-c-g")
```

The `textreuse` library helps us here. It treats each phoneme as a word
and tries align these words.

``` r
library(textreuse)
library(purrr)
align_local(x, y)
#> TextReuse alignment
#> Alignment score: 40 
#> Document A:
#> dh 4 b 3 d l ae n d I d b i h I n d dh 4 sh ae # g # i d c g
#> 
#> Document B:
#> dh 4 b 3 d l ae n d I d b i h I n d ## # sh ## e # k i d c g
```

But note that the nontext characters in “@I” and “3^” disappear. For
that reason, it would be helpful to do a pass that replace
punctuation-based fake IPA with word-character-based fake IPA. Let’s
make something to replace the nontext characters with IPA. We will
eventually need to write something to comprehensively work in IPA.

``` r
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
  
x <- fix_ipa(x)
y <- fix_ipa(y)

align_local(x, y)
#> TextReuse alignment
#> Alignment score: 40 
#> Document A:
#> dh 4 b 3r d l ae n d I d b i h aI n d dh 4 sh ae # g # i d c g
#> 
#> Document B:
#> dh 4 b 3r d l ae n d I d b i h aI n d ## # sh ## e # k i d c g
```

One more thing we should is a function to print the alignments with bars
like the original ShowAndTell program did.

``` r
to_chars <- function(x) {
  x %>% 
    strsplit(" ") %>% 
    unlist()
}

print_alignment <- function(alignment) {
  matches <- to_chars(alignment$a_edits) == to_chars(alignment$b_edits)
  lengths <- nchar(to_chars(alignment$a_edits))
  aligners <- ifelse(matches, "|", " ")

  marks <- list_along(aligners) %>%
    map2(lengths, function(x, y) paste0(rep(" ", y), collapse = "")) %>%
    map2(aligners, function(x, y) paste0(y, x)) %>%
    unlist() %>%
    paste0(collapse = "")

  cat(alignment$a_edits, marks, alignment$b_edits, sep = "\n")
  invisible(alignment)
}

align_local(x, y) %>% 
  print_alignment()
#> dh 4 b 3r d l ae n d I d b i h aI n d dh 4 sh ae # g # i d c g
#> |  | | |  | | |  | | | | | | | |  | |      |           | | | | 
#> dh 4 b 3r d l ae n d I d b i h aI n d ## # sh ## e # k i d c g
```

This looks really good so far. This alignment procedure leaves some
information on the table, however: There is no credit for partial
matching. The algorithm gives n-points for a “sh” matching in
shaggy/shaky, but the “g”/“k” is not arbitrary non-match. That should be
worth some partial credit based on how the two only differ in one
phonetic feature. But let’s hold this thought.

One thing that is missing is some treatment of spaces between words. We
would like to use those when scoring alignments too. But let’s hold this
thought too.

### the challenge example

Our first priority is to fix the algorithm to handle the next example.

``` r
x2 <- c(
  # point to teddy
  "p-cI-n-t t-u t-E-d-I")
y2 <- c(
  # point teddy
  "p-cI-n-t t-E-d-I")

align_local(x2, y2) %>% 
  print_alignment()
#> p cI n t t u t E d I
#> | |  | | |     | | | 
#> p cI n t t # # E d I
```

The problem here is that “t” is assigned to “to” instead of “teddy” so
that it seems like the child said “point t eddy”.

One possibility is to insert word-spaces as explicit words that can be
aligned, but that doesn’t help.

``` r
str_mark_space <- function(x, replace = "space") {
  stringr::str_replace_all(x, " ", paste0(" ", replace, " "))
}

# doesn't help
align_local(
  str_mark_space(x2), 
  str_mark_space(y2)) %>% 
  print_alignment()
#> p cI n t space t u space t E d I
#> | |  | | |     |           | | | 
#> p cI n t space t # ##### # E d I
```

## bigramify

I think that aligning phonemes might work easier if we do a first pass
on bigrams. As a matter of good practice, let’s write a general
n-gramming function.

``` r
str_tokenize <- function(x) {
  stringr::str_split(x, " ")[[1]]
}

str_ngramify <- function(x, n) {
  x %>% 
    str_tokenize() %>% 
    map_chr(str_ngramify_word, n) %>% 
    stringr::str_flatten(" ")
}

str_ngramify_word <- function(word, n) {
  dashes <- seq_len(stringr::str_count(word, "-"))
  
  for (dash_i in dashes) {
    if (dash_i %% n == 1) {
      word <- stringr::str_replace(word, "-", "x22x")
    } else {
      word <- stringr::str_replace(word, "-", "xskipx")
    }
  }
  stringr::str_replace_all(word, "xskipx", "-")
}


# make_ngram_matches_words <- function(alignment) {
#   a <- alignment$a_edits %>% str_tokenize()
#   b <- alignment$b_edits %>% str_tokenize()
#   edits <- a %>% seq_along()
#   for (edit_i in edits) {
#     if (a[edit_i] == b[edit_i]) {
#       a[edit_i] <- str_ungramify(a[edit_i])
#       b[edit_i] <- str_ungramify(b[edit_i])
#     } else {
#       a[edit_i] <- str_ungramify(a[edit_i], unjoin = "-")
#       b[edit_i] <- str_ungramify(b[edit_i], unjoin = "-")
#     }
#   } 
#   alignment$a_edits <- stringr::str_flatten(a, " ")
#   alignment$b_edits <- stringr::str_flatten(b, " ")
#   alignment
# }

make_bigram_matches_words <- function(alignment) {
  # The trick here is to make a matching bigram into 4 matching words
  str_ungramify2 <- function(x) {
    x %>% 
      stringr::str_replace_all(
        "(\\w+)(x22x)(\\w+)", "startngram \\1-\\3 endngram")
  }
  
  a <- alignment$a_edits %>% str_tokenize()
  b <- alignment$b_edits %>% str_tokenize()
  edits <- a %>% seq_along()
  for (edit_i in edits) {
    if (a[edit_i] == b[edit_i]) {
      a[edit_i] <- str_ungramify2(a[edit_i])
      b[edit_i] <- str_ungramify2(b[edit_i])
    } else {
      a[edit_i] <- str_ungramify(a[edit_i], unjoin = "-")
      b[edit_i] <- str_ungramify(b[edit_i], unjoin = "-")
    }
  } 
  
  
  alignment$a_edits <- stringr::str_flatten(a, " ")
  alignment$b_edits <- stringr::str_flatten(b, " ")
  alignment
}

remove_edit_marks <- function(alignment) {
  clean_up <- . %>% 
    stringr::str_remove_all("#+") %>% 
    stringr::str_replace_all("[ ]+", " ")
    
  alignment$a_edits <- clean_up(alignment$a_edits)
  alignment$b_edits <- clean_up(alignment$b_edits)
  
  alignment
}


str_ungramify <- function(x, join = "x22x", skip = "xskip", 
                          unjoin = " ngram ", unskip = "-") {
  x %>% 
    stringr::str_replace_all(join, unjoin) %>% 
    stringr::str_replace_all(skip, unskip)
}

realign <- function(alignment, match = 4L, ...) {
  align_local(alignment$a_edits, alignment$b_edits, match = match, ...)
}


finalize_alignment <- function(alignment) {
  clean_word <- function(x, y) {
    x %>% 
      stringr::str_replace("space", "_") %>% 
      stringr::str_replace("startngram", "") %>%  
      stringr::str_replace("endngram", "") %>%  
      stringr::str_replace("ngram", "") 
  }
  
  clean_pair <- function(x, y) {
    x <- clean_word(x)
    y <- clean_word(y)
    if (stringr::str_detect(x, "#")) {
      x <- stringr::str_flatten(rep("#", nchar(y)))
      if (nchar(y) == 0) {
        x <- ""
      } 
    } else if (stringr::str_detect(y, "#")) {
      y <- stringr::str_flatten(rep("#", nchar(x)))
      if (nchar(x) == 0) {
        y <- ""
      } 
    }
    list(x, y)
  }
  
  a <- alignment$a_edits %>% str_tokenize()
  b <- alignment$b_edits %>% str_tokenize()
  
  edits <- a %>% seq_along()
  for (edit_i in edits) {
    if (a[edit_i] == b[edit_i]) {
      a[edit_i] <- clean_word(a[edit_i])
      b[edit_i] <- clean_word(b[edit_i])
    } else {
      x <- a[edit_i]
      y <- b[edit_i]
      
      a_b <- clean_pair(a[edit_i], b[edit_i])
      a[edit_i] <- a_b[[1]]
      b[edit_i] <- a_b[[2]]
    }
  } 
  
  a <- a[a != ""]
  b <- b[b != ""]
  alignment$a_edits <- stringr::str_flatten(a, " ")
  alignment$b_edits <- stringr::str_flatten(b, " ")
  alignment
}
```

Let’s run through the whole process.

``` r
align_bigrams <- function(x, y, match = 4L, ...) {
  x1 <- x %>% str_mark_space() %>% str_ngramify(2)
  y1 <- y %>% str_mark_space() %>% str_ngramify(2)
  
  alignment <- align_local(x1, y1, match = 4L) 
  
  alignment %>% 
    make_bigram_matches_words() %>% 
    remove_edit_marks() %>% 
    realign(match = 4L) %>% 
    finalize_alignment()
}
```

Back to the hard one…

``` r
align_bigrams(x2, y2) %>% 
  print_alignment()
#> p cI n t _ t u _ t E d I
#> | |  | | |       | | | | 
#> p cI n t _ # # # t E d I
```

## The next problem is IPA

The following works interactively, but it doesn’t work when I render
this report.

``` r
phone1 <- c("ð-ə b-ɜr-d")
phone2 <- c("ð-ə b-ɜr-d")

print(phone1)
#> [1] "ð-<U+0259> b-<U+025C>r-d"

align_bigrams(phone1, phone2)
#> TextReuse alignment
#> Alignment score: 36 
#> Document A:
#> ðx22x U 0259 _ bx22x U 025C r d
#> 
#> Document B:
#> ðx22x U 0259 _ bx22x U 025C r d
```

The output should be:

    TextReuse alignment
    Alignment score: 40 
    Document A:
    ð ə _ b ɜr d
    
    Document B:
    ð ə _ b ɜr d

Note that the alignment scores are different.

## More testing

``` r
tests <- yaml::read_yaml("align.yaml")

for (test in tests) {
  clean_input <- . %>% 
    fix_ipa() %>% 
    stringr::str_replace_all("-+", "-") %>% 
    stringr::str_replace_all(" -", " ") %>% 
    stringr::str_replace_all("- ", " ")
  
  x <- test$char_a %>% 
    clean_input()
  y <- test$char_b %>% 
    clean_input()
  
  cat(test$gloss_a, "\n")
  cat(test$char_a, "\n")
  align_bigrams(x, y) %>% 
    print_alignment()
  cat(test$char_b, "\n")
  cat(test$gloss_b, "\n")

  cat("\n")

}
#> He gave mommy a birthday card 
#> h-i g-eI-v m-@-m-i 4 b-3^-th-d-eI k-@-r-d 
#> h i _ g eI # v _ m a m i _ 4 _ b 3r th d eI _ k a r d
#> | | | |      | | | | | | | | | | |  |  | |  | | | | | 
#> h i _ g ## I v _ m a m i _ 4 _ b 3r th d eI _ k a r d
#> h-i g-I--v m-@-m-i 4 b-3^-th-d-eI k-@-r-d 
#> he give mommy a birthday card 
#> 
#> Baby likes his new toy 
#> b-eI-b-i l-@I-k-s h-I-z n-u t-cI 
#> b eI b i _ l aI k s _ h I z _ n u _ t cI
#> | |  | | | | |  | | | | | | | | | | | |  
#> b eI b i _ l aI k s _ h I z _ n u _ t cI
#> b-eI-b-i l-@I-k-s h-I-z n-u t-cI 
#> baby likes his new toy 
#> 
#> Doctor bag 
#> d-@-k-t-4^ b-ae-g 
#> d a k t 4r _ b ae # g
#> | | | | |  | |      | 
#> d a k t 4r _ b ## I g
#> d-@-k-t-4^ b-I--g 
#> doctor big 
#> 
#> The bird landed behind the shaggy dog 
#> dh-4 b-3^-d l-ae-n-d-I-d b-i-h-@I-n-d dh-4 sh-ae-g-i d-c-g 
#> dh 4 _ b 3r d _ l ae n d I d _ b i h aI n d _ dh 4 _ sh ae g # i _ d c g
#> |  | | | |  | | | |  | | | | | | | | |  | | | |  | | |  |        | | | | 
#> dh 4 _ b 3r d _ l ae n d I d _ b i h aI n d _ dh 4 _ sh ae # k I _ d c g
#> dh-4 b-3^-d l-ae-n-d-I-d b-i-h-@I-n-d dh-4 sh-ae-k-I d-c-g 
#> the bird landed behind the shaky dog 
#> 
#> There's a playground near the school 
#> dh-E-r-z 4 p-l-eI-g-r-@U-n-d n-i-r dh-4 s-k-u-l 
#> dh E r z _ 4 _ p l eI g r aU n d _ n i r _ dh 4 _ s k u l
#> |  | | | | | | | | |  | | |  | | | | | | | |  | | | | | | 
#> dh E r z _ 4 _ p l eI g r aU n d _ n i r _ dh 4 _ s k u l
#> dh-E-r-z 4 p-l-eI-g-r-@U-n-d n-i-r dh-4 s-k-u-l 
#> there's a playground near the school 
#> 
#> Open the house 
#> oU-p-4-n dh-4 h-@U-s 
#> oU p 4 n _ dh 4 _ h aU s
#> |  | | | | |  | | | |  | 
#> oU p 4 n _ dh 4 _ h aU s
#> oU-p-4-n dh-4 h-@U-s 
#> open the house 
#> 
#> Hug daddy 
#> h-^-g d-ae-d-i 
#> h A g _ d ae d i
#> | | | | | |  | | 
#> h A g _ d ae d i
#> h-^-g d-ae-d-i 
#> hug daddy 
#> 
#> Point to Teddy 
#> p-cI-n-t t-u t-E-d-I 
#> p cI n t _ t u _ t E d I
#> | |  | | |       | | | | 
#> p cI n t _ # # # t E d I
#> p-cI-n-t ----t-E-d-I 
#> point teddy 
#> 
#> They ate birthday cake and drank pop 
#> dh-eI eI-t b-3^-th-d-eI k-eI-k ae-n-d-d-r-ae-ng-k p-@-p 
#> dh eI _ eI t _ b 3r th d eI _ k eI k _ ae n d d r ae # ng k _ p a p
#> |  |  | |  | | | |  |  | |  | | |  | | |  | | | |      |  | | | | | 
#> dh eI _ eI t _ b 3r th d eI _ k eI k _ ae n d d r ## I ng k _ p a p
#> dh-eI eI-t b-3^-th-d-eI k-eI-k ae-n-d-d-r-I--ng-k p-@-p 
#> they ate birthday cake and drink pop 
#> 
#> Tie those shoes 
#> t-@I dh-oU-z sh-u-z 
#> t aI _ dh oU z _ sh u z
#> | |  | |  |  | | |  | | 
#> t aI _ dh oU z _ sh u z
#> t-@I dh-oU-z sh-u-z 
#> tie those shoes 
#> 
#> Get off 
#> g-E-t c-f 
#> g E t _ c f
#> | | | | | | 
#> g E t _ c f
#> g-E-t c-f 
#> get off 
#> 
#> That is her dog 
#> dh-ae-t I-z h-3^ d-c-g 
#> dh ae t _ I z _ h 3r _ d c g
#> |  |  | | | | | | |  | | | | 
#> dh ae t _ I z _ h 3r _ d c g
#> dh-ae-t I-z h-3^ d-c-g 
#> that is her dog 
#> 
#> The sign says keep out 
#> dh-4 s-@I-n s-E-z k-i-p @U-t 
#> dh 4 _ s aI n _ s E z _ k i p _ aU t
#> |  | | | |  | | | | | | | | | | |  | 
#> dh 4 _ s aI n _ s E z _ k i p _ aU t
#> dh-4 s-@I-n s-E-z k-i-p @U-t 
#> the sign says keep out 
#> 
#> Say goodbye 
#> s-eI g-U-d--b-@I 
#> s eI _ g U
#> | |  | | | 
#> s eI _ g U
#> s-eI g-U-d  b-@I 
#> say good-bye 
#> 
#> They are singing happy birthday 
#> dh-eI @-r s-I-ng-I-ng h-ae-p-I b-3^-th-d-eI 
#> _ s I ng I ng _ h ae p I _ b 3r th d eI
#> | | | |  | |  | | |  | | | | |  |  | |  
#> _ s I ng I ng _ h ae p I _ b 3r th d eI
#> dh-E----r s-I-ng-I-ng h-ae-p-I b-3^-th-d-eI 
#> they're singing happy birthday 
#> 
#> Water shoots from that gun 
#> w-@-t-4^ sh-u-t-s f-r-^-m dh-ae-t g-^-n 
#> w a t 4r _ sh u t s _ f r A m _ dh ae t _ g A n
#> | | | |  | |  | | | | | | | | | |  |  | | | | | 
#> w a t 4r _ sh u t s _ f r A m _ dh ae t _ g A n
#> w-@-t-4^ sh-u-t-s f-r-^-m dh-ae-t g-^-n 
#> water shoots from that gun 
#> 
#> She chewed her fingers 
#> sh-i tsh-u-d h-3^ f-I-ng-g-4^-z 
#> sh i _ tsh u d _ h 3r _ f I ng g 4r z
#> |  | | |   | | | | |  | | | |  | |  | 
#> sh i _ tsh u d _ h 3r _ f I ng g 4r z
#> sh-i tsh-u-d h-3^ f-I-ng-g-4^-z 
#> she chewed her fingers 
#> 
#> Baby reached to get another chocolate cookie 
#> b-eI-b-i r-i-tsh-t t-u g-E-t 4-n-^-dh-4^ tsh-@-k-l-I-t k-U-k-i 
#> b eI b i _ r i tsh t _ t u _ g E t _ 4 n A dh 4r _ tsh a k l I t _ k U k i
#> | |  | | | | | |   | | | | | | | | | | | | |  |  | |   | | | | | | | | | | 
#> b eI b i _ r i tsh t _ t u _ g E t _ 4 n A dh 4r _ tsh a k l I t _ k U k i
#> b-eI-b-i r-i-tsh-t t-u g-E-t 4-n-^-dh-4^ tsh-@-k-l-I-t k-U-k-i 
#> baby reached to get another chocolate cookie 
#> 
#> Gather the toys 
#> g-ae-dh-4^ dh-4 t-cI-z 
#> g ae dh 4r _ dh 4 _ t cI z
#> | |  |  |  | |  | | | |  | 
#> g ae dh 4r _ dh 4 _ t cI z
#> g-ae-dh-4^ dh-4 t-cI-z 
#> gather the toys 
#> 
#> Pull his tooth 
#> p-U-l h-I-z t-u-th 
#> p U l _ h I z _ t u th
#> | | | | | | | | | | |  
#> p U l _ h I z _ t u th
#> p-U-l h-I-z t-u-th 
#> pull his tooth 
#> 
#> Pour some tea from both of us 
#> p-o-r s-^-m t-i f-r-^-m b-oU-th ^-v ^-s 
#> p o r _ s A m _ t i _ f # r A m _ b oU th _ A v _ A s
#> | | | | | | | | | | | |   |     | | |  |  | | | | | | 
#> p o r _ s A m _ t i _ f c r # # _ b oU th _ A v _ A s
#> p-o-r s-^-m t-i f---c-r b-oU-th ^-v ^-s 
#> pour some tea for both of us 
#> 
#> Big potato 
#> b-I-g p-4-t-eI-t-o 
#> b I g _ p 4 t eI t o
#> | | | | | | | |  | | 
#> b I g _ p 4 t eI t o
#> b-I-g p-4-t-eI-t-o 
#> big potato 
#> 
#> Cut two small pieces 
#> k-^-t t-u s-m-c-l p-i-s-I-z 
#> _ s m c l _ p i s I z
#> | | | | | | | | | | | 
#> _ s m c l _ p i s I z
#> ------t-u s-m-c-l p-i-s-I-z 
#> two small pieces 
#> 
#> She is showing him how to jump 
#> sh-i I-z sh-oU-I-ng-h-I-m h-@U t-u dzh-^-m-p 
#> sh i _ I z _ sh oU I ng h I m _ h aU _ t u _ dzh A m p
#> |  | | | | | |  |  | |  | | | | | |  | | | | |   | | | 
#> sh i _ I z _ sh oU I ng h I m _ h aU _ t u _ dzh A m p
#> sh-i I-z sh-oU-I-ng-h-I-m h-@U t-u dzh-^-m-p 
#> she is showing him how to jump 
#> 
#> His fingers are in wrong 
#> h-I-z f-I-ng-g-4^-z @-r I-n r-c-ng 
#> h I z _ f I ng g 4r z _ a r _ I n _ r c ng
#> | | | | | | |  | |  | | | | | | | | | | |  
#> h I z _ f I ng g 4r z _ a r _ I n _ r c ng
#> h-I-z f-I-ng-g-4^-z @-r I-n r-c-ng 
#> his fingers are in wrong 
#> 
#> Tie up the garbage bag 
#> t-@I ^-p dh-4 g-@-r-b-4-dzh-b-ae-g 
#> t aI _ A p _ dh 4 _ g a r b 4 dzh b ae g
#> | |  | | | | |  | | | | | | | |   | |  | 
#> t aI _ A p _ dh 4 _ g a r b 4 dzh b ae g
#> t-@I ^-p dh-4 g-@-r-b-4-dzh-b-ae-g 
#> tie up the garbage bag 
#> 
#> Wear a cowboy hat 
#> w-E-r 4 k-@U-b-cI h-ae-t 
#> w E r _ 4 _ k aU b cI _ h ae t
#> | | | | | | | |  | |  | | |  | 
#> w E r _ 4 _ k aU b cI _ h ae t
#> w-E-r 4 k-@U-b-cI h-ae-t 
#> wear a cowboy hat 
#> 
#> She laughed at his funny jokes 
#> sh-i l-ae-f-t ae-t h-I-z f-^-n-I dzh-oU-k-s 
#> sh i _ l ae f t _ ae t _ h I z _ f A n I _ dzh oU k s
#> |  | | | |  | | | |  | | | | | | | | | | | |   |  | | 
#> sh i _ l ae f t _ ae t _ h I z _ f A n I _ dzh oU k s
#> sh-i l-ae-f-t ae-t h-I-z f-^-n-I dzh-oU-k-s 
#> she laughed at his funny jokes 
#> 
#> Point to Teddy 
#> p-cI-n-t---  t-u t-E-d-I 
#> p cI n t # # _ _ t u _ t E d I
#> | |  | |             | | | | | 
#> p cI n t 4 d # # # # _ t E d I
#> p-cI-n-t-4-d ----t-E-d-I 
#> pointed teddy 
#> 
#> Those eat those hotdogs soon 
#> dh-oU-z i-t dh-oU-z h-@-t--d-c-g-z  s-u-n
#> Warning: Multiple optimal local alignments found; selecting only one of
#> them.
#> _
#> | 
#> _
#> --------------------h-@-t  d-c-g-z------ 
#> hot dogs 
#> 
#> Find the brown lid 
#> f-@I-n-d dh-4 b-r-@U-n l-I-d 
#> _ l I d
#> | | | | 
#> _ l I d
#> --------------b-r-@U-n l-I-d 
#> brown lid 
#> 
#> She got mad and pushed the boy 
#> sh-i g-@-t m-ae-d ae-n-d-p-U-sh-t dh-4 b-cI 
#> sh i _ g a t _ m ae d _ ae n d p U sh t _ dh 4 _ b cI
#> |  | | | | | | | |  | | |  | | | | |  | | |  | | | |  
#> sh i _ g a t _ m ae d _ ae n d p U sh t _ dh 4 _ b cI
#> sh-i g-@-t m-ae-d ae-n-d-p-U-sh-t dh-4 b-cI 
#> she got mad and pushed the boy 
#> 
#> Be very quiet when baby is sleeping 
#> b-i v-E-r-I k-w-@I-4-t w-E-n b-eI-b-i I-z s-l-i-p-l-ng 
#> b i _ v E r I _ k w aI 4 t _ w E n _ b eI b i _ I z _ s l i p l ng
#> | | | | | | | | | | |  | | | | | | | | |  | | | | | | | | | | | |  
#> b i _ v E r I _ k w aI 4 t _ w E n _ b eI b i _ I z _ s l i p l ng
#> b-i v-E-r-I k-w-@I-4-t w-E-n b-eI-b-i I-z s-l-i-p-l-ng 
#> be very quiet when baby is sleeping 
#> 
#> She could hide here 
#> sh-i k-U--d h-@I-d h-i-r 
#> sh i _ k U ## d # _ h aI d _ h i r
#> |  | | |          | | |  | | | | | 
#> sh i _ k # ae # n _ h aI d _ h i r
#> sh-i k-ae-n h-@I-d h-i-r 
#> she can hide here 
#> 
#> Take the lid off the pot 
#> t-eI-k dh-4 l-I-d c-f dh-4 p-@-t 
#> t eI k _ dh 4 _ l I d _ c f _ dh 4 _ p a t
#> | |  | | |  | | | | | | | | | |  | | | | | 
#> t eI k _ dh 4 _ l I d _ c f _ dh 4 _ p a t
#> t-eI-k dh-4 l-I-d c-f dh-4 p-@-t 
#> take the lid off the pot 
#> 
#> Get them some coffee 
#> g-E-t dh-E-m s-^-m k-c-f-i 
#> g E t _ dh E m _ s A m _ k c f i
#> | | | | |  | | | | | | | | | | | 
#> g E t _ dh E m _ s A m _ k c f i
#> g-E-t dh-E-m s-^-m k-c-f-i 
#> get them some coffee 
#> 
#> Take his turn 
#> t-eI-k-  h-I-z t-3^-n- 
#> t eI k # _ _ h I z _ t 3r
#> | |  |     | | | | | | |  
#> t eI k s # _ h I z _ t 3r
#> t-eI-k-s h-I-z t-3^-n-z 
#> takes his turns 
#> 
#> Don't let go or it will fall 
#> d-o-n-t l-E-t g-oU c-r I-t w-I-l f-c-l 
#> d o n t _ l E t _ g oU _ c r _ I t _ w I l _ f c l
#> | | | | | | | | | | |  | | | | | | | | | | | | | | 
#> d o n t _ l E t _ g oU _ c r _ I t _ w I l _ f c l
#> d-o-n-t l-E-t g-oU c-r I-t w-I-l f-c-l 
#> don't let go or it will fall 
#> 
#> Make a check behind the two 
#> m-eI-k --4--  tsh-E-k b-i-h-@I-n-d dh-4  t-u 
#> # # # ## # _ tsh E k _ b i h aI n d _ dh 4
#>            | |   | | | | | | |  | | | |  | 
#> _ _ k ae n _ tsh E k _ b i h aI n d _ dh 4
#> --@I-  k-ae-n tsh-E-k b-i-h-@I-n-d dh-4---- 
#> i can check behind the 
#> 
#> He wants somebody to push him 
#> h-i w-^-n-t-s s-^-m-b-^-d-i t-u p-U-sh h-I-m 
#> h i _ w A n t s _ s A m b A d i _ t u _ p U sh _ h I m
#> | | | | | | | | |                 | | | | | |  | | | | 
#> h i _ w A n t s _ # # # # # # # # t u _ p U sh _ h I m
#> h-i w-^-n-t-s --------------t-u p-U-sh h-I-m 
#> he wants to push him 
#> 
#> They ate birthday cake and drank pop 
#> dh-eI eI-t b-3^-th-d-eI k-eI-k ae-n-d-d-r-ae-ng-k p-@-p 
#> dh eI _ eI t _ b 3r th d eI _ k eI k _ ae n d d r ae # ng k _ p a p
#> |  |  | |  | | | |  |  | |  | | |  | | |  | | | |      |  | | | | | 
#> dh eI _ eI t _ b 3r th d eI _ k eI k _ ae n d d r ## I ng k _ p a p
#> dh-eI eI-t b-3^-th-d-eI k-eI-k ae-n-d-d-r-I--ng-k p-@-p 
#> they ate birthday cake and drink pop 
#> 
#> Living room 
#> l-I-v-I-ng r-u-m 
#> l I v I ng _ r u m
#> | | | | |  | | | | 
#> l I v I ng _ r u m
#> l-I-v-I-ng r-u-m 
#> living room 
#> 
#> Animal crackers 
#> ae-n-I-m-4-l k-r-ae-k-4^-s 
#> ae n I m 4 l _ k r ae k 4r s
#> |  | | | | | | | | |  | |  | 
#> ae n I m 4 l _ k r ae k 4r s
#> ae-n-I-m-4-l k-r-ae-k-4^-s 
#> animal crackers 
#> 
#> The loud noise scared the scared the cat away 
#> dh-4 l-@U-d n-cI-z s-k-E-4^-d-dh-4 s-k-E-4^-d-dh-4 k-ae-t 4-w-eI 
#> dh 4 _ l aU d _ n cI z _ s k E 4r d dh 4 _ s k E 4r d dh 4 _ k ae t _ 4 w eI
#> |  | | | |  | | | |  | | | | | |  | |  | |                   | |  | | | | |  
#> dh 4 _ l aU d _ n cI z _ s k E 4r d dh 4 _ # # # ## # ## # # k ae t _ 4 w eI
#> dh-4 l-@U-d n-cI-z s-k-E-4^-d-dh-4 ----------------k-ae-t 4-w-eI 
#> the loud noise scared the cat away 
#> 
#> Five more cookies 
#> f-@I-v m-o-r k-U-k-i-z 
#> f aI v _ m o r _ k U k i z
#> | |  | |         | | | | | 
#> f aI v _ # # # # k U k i z
#> f-@I-v ------k-U-k-i-z 
#> five cookies 
#> 
#> That baby is learning to talk 
#> dh-ae-t b-eI-b-i I-z l-3^-n-I-ng t-u  t-c-k 
#> dh ae t _ b eI b i _ I z _ l 3r n I ng _ t u
#> |  |  | | | |  | | | | | | | |  | | |  | | | 
#> dh ae t _ b eI b i _ I z _ l 3r n I ng _ t u
#> dh-ae-t b-eI-b-i I-z l-3^-n-I-ng t-u------ 
#> that baby is learning to 
#> 
#> Make a birdhouse 
#> m-eI-k 4 b-3^-d-h-@U-s 
#> m eI k _ 4 _ b 3r d h aU s
#> | |  | | | | | |  | | |  | 
#> m eI k _ 4 _ b 3r d h aU s
#> m-eI-k 4 b-3^-d-h-@U-s 
#> make a birdhouse 
#> 
#> Baby sock 
#> b-eI-b-i  s-@-k 
#> b eI b i
#> | |  | | 
#> b eI b i
#> b-eI-b-i------ 
#> baby 
#> 
#> Give some flowers some water 
#> g-I-v s-^-m f-l-@U-4^-z s-^-m w-@-t-4^ 
#> g I v _ s A m _ f l aU 4r z _ s A m _ w a t 4r
#> | | | | | | | | | | |  |  | | | | | | | | | |  
#> g I v _ s A m _ f l aU 4r z _ s A m _ w a t 4r
#> g-I-v s-^-m f-l-@U-4^-z s-^-m w-@-t-4^ 
#> give some flowers some water 
#> 
#> This cheese doesn't smell good 
#> dh-I-s tsh-i-z d-^-z-n-t s-m-E-l g-U-d 
#> _ d A z n t _ s m E l _ g U d
#> | | | | | | | | | | | | | | | 
#> _ d A z n t _ s m E l _ g U d
#> -------tsh-i-z d-^-z-n-t s-m-E-l g-U-d 
#> cheese doesn't smell good 
#> 
#> He winds up the toy ghost 
#> h-i w-@I-n-d-z ^-p dh-4 t-cI g-oU-s-t 
#> h i _ w aI n d z _ A p _ dh 4 _ t cI _ g oU s t
#> | | | | |  | | | | | | | |  | | | |  | | |  | | 
#> h i _ w aI n d z _ A p _ dh 4 _ t cI _ g oU s t
#> h-i w-@I-n-d-z ^-p dh-4 t-cI g-oU-s-t 
#> he winds up the toy ghost 
#> 
#> Open the house 
#> oU-p-4-n dh-4 h-@U-s 
#> oU p 4 n _ dh 4 _ h aU s
#> |  | | | | |  | | | |  | 
#> oU p 4 n _ dh 4 _ h aU s
#> oU-p-4-n dh-4 h-@U-s 
#> open the house 
#> 
#> He likes potato chips 
#> h-i l-@I-k-s p-4-t-eI-t-o tsh-I-p-s 
#> h i _ l aI k s _ p 4 t eI t o _ tsh I p s
#> | | | | |  | | | | | | |  | | | |   | | | 
#> h i _ l aI k s _ p 4 t eI t o _ tsh I p s
#> h-i l-@I-k-s p-4-t-eI-t-o tsh-I-p-s 
#> he likes potato chips 
#> 
#> Taste the cookies that she baked 
#> t-eI-s-t dh-4 k-U-k-i-z dh-ae-t sh-i b-eI-k-t 
#> _ dh ae t _ sh i _ b eI k t
#> | |  |  | | |  | | | |  | | 
#> _ dh ae t _ sh i _ b eI k t
#> --------------k-U-k-i-z dh-ae-t sh-i b-eI-k-t 
#> cookies that she baked 
#> 
#> Jump over the box 
#> dzh-^-m-p oU-v-4^ dh-4 b-@-k-s 
#> dzh A m p _ oU v 4r _ dh 4 _ b a k s
#> |   | | | | |  | |  | |  | | | | | | 
#> dzh A m p _ oU v 4r _ dh 4 _ b a k s
#> dzh-^-m-p oU-v-4^ dh-4 b-@-k-s 
#> jump over the box 
#> 
#> Both faces are happy 
#> b-oU-th f-eI-s-4-z @-r h-ae-p-I 
#> b oU th _ f eI s 4 z _ a r _ h ae p I
#> | |  |  |              | | | | |  | | 
#> b oU th _ # ## # # # # a r _ h ae p I
#> b-oU-th -----------@-r h-ae-p-I 
#> both are happy 
#> 
#> Fill the fridge with things to eat 
#> f-I-l dh-4 f-r-I-dzh w-I-th th-I-ng-z t-u i-t 
#> f I l _ dh 4 _ f r I dzh _ w I th _ th I ng z _ t u _ i t
#> | | | | |  | | | | | |   | | | |  | |  | |  | | | | | | | 
#> f I l _ dh 4 _ f r I dzh _ w I th _ th I ng z _ t u _ i t
#> f-I-l dh-4 f-r-I-dzh w-I-th th-I-ng-z t-u i-t 
#> fill the fridge with things to eat 
#> 
#> Put these together 
#> p-U-t dh-i-z t-u-g-E-dh-4^ 
#> p U t _ dh i z _ t u g E dh 4r
#> | | | | |  | | | | | | | |  |  
#> p U t _ dh i z _ t u g E dh 4r
#> p-U-t dh-i-z t-u-g-E-dh-4^ 
#> put these together 
#> 
#> The cowboy hat has a feather 
#> dh-4 k-@U-b-cI h-ae-t h-ae-z 4 f-E-dh-4^ 
#> dh 4 _ k aU b cI _ h ae t _ h ae z _ 4 _ f E dh 4r
#> |  | | | |  | |  | | |  | | | |  | |     | | |  |  
#> dh 4 _ k aU b cI _ h ae t _ h ae z _ # # f E dh 4r
#> dh-4 k-@U-b-cI h-ae-t h-ae-z --f-E-dh-4^ 
#> the cowboy hat has feather 
#> 
#> Bird house 
#> b-3^-d h-@U-s 
#> b 3r d _ h aU s
#> | |  | | | |  | 
#> b 3r d _ h aU s
#> b-3^-d h-@U-s 
#> bird house 
#> 
#> Cowboy boots 
#> k-@U-b-cI b-u-t-s 
#> k aU b cI _ b u t s
#> | |  | |  | | | | | 
#> k aU b cI _ b u t s
#> k-@U-b-cI b-u-t-s 
#> cowboy boots 
#> 
#> There's a playground near the school 
#> dh-E-r-z 4 p-l-eI-g-r-@U-n-d n-i-r dh-4 s-k-u-l 
#> dh E r z _ 4 _ p l eI g r aU n d _ n i r _ dh 4 _ s k u l
#> |  | | | | | | | | |  | | |  | | | | | | | |  | | | | | | 
#> dh E r z _ 4 _ p l eI g r aU n d _ n i r _ dh 4 _ s k u l
#> dh-E-r-z 4 p-l-eI-g-r-@U-n-d n-i-r dh-4 s-k-u-l 
#> there's a playground near the school 
#> 
#> Don't let go or it will fall 
#> d-o-n-t l-E-t g-oU c-r I-t w-I-l f-c-l 
#> d o n t _ l E t _ g oU _ c r _ I t _ w I l _ f c l
#> | | | | | | | | | | |  | | | | | | | | | | | | | | 
#> d o n t _ l E t _ g oU _ c r _ I t _ w I l _ f c l
#> d-o-n-t l-E-t g-oU c-r I-t w-I-l f-c-l 
#> don't let go or it will fall 
#> 
#> Give some flowers some water 
#> g-I-v s-^-m f-l-@U-4^-z s-^-m w-@-t-4^ 
#> g I v _ s A m _ f l aU 4r z _ s A m _ w a t 4r
#> | | | | | | | | | | |  |  | | | | | | | | | |  
#> g I v _ s A m _ f l aU 4r z _ s A m _ w a t 4r
#> g-I-v s-^-m f-l-@U-4^-z s-^-m w-@-t-4^ 
#> give some flowers some water 
#> 
#> That's not white 
#> dh-ae-t-s n-@-t w-@I-t 
#> dh ae t s _ n a t _ w # aI t
#> |  |  | | | | | | |     |  | 
#> dh ae t s _ n a t _ # r aI t
#> dh-ae-t-s n-@-t r-@I-t 
#> that's not right 
#> 
#> Boy 
#> b-cI 
#> b cI
#> | |  
#> b cI
#> b-cI 
#> boy 
#> 
#> Chew 
#> tsh-u 
#> tsh u
#> |   | 
#> tsh u
#> tsh-u 
#> chew 
#> 
#> Bee 
#> b-i 
#> b i
#> | | 
#> b i
#> b-i 
#> bee 
#> 
#> Ball 
#> b-c-l 
#> b c l
#> | | | 
#> b c l
#> b-c-l 
#> ball 
#> 
#> Walk 
#> w-c-k 
#> w c k
#> | | | 
#> w c k
#> w-c-k 
#> walk 
#> 
#> Yawn 
#> j-c-n 
#> j c n
#> | | | 
#> j c n
#> j-c-n 
#> yawn 
#> 
#> Snow 
#> s-n-oU 
#> s n oU
#> | | |  
#> s n oU
#> s-n-oU 
#> snow 
#> 
#> Beanie 
#> b-i-n-i- 
#> b i n i
#> |   | | 
#> b I n i
#> b-I-n-i-th 
#> beneath 
#> 
#> Boot 
#> b-u-t 
#> b u t
#> | | | 
#> b u t
#> b-u-t 
#> boot 
#> 
#> Mud 
#> m-^-d 
#> m A d
#> | | | 
#> m A d
#> m-^-d 
#> mud 
#> 
#> A Dress 
#> 4   d-r-E-s
#> Warning: Multiple optimal local alignments found; selecting only one of
#> them.
#> Error in b_out[out_i] <- b_orig[row_i - 1]: replacement has length zero
```

Looks like I need to fix cases where many words are different…
