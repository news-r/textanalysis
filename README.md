
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
[![Travis build status](https://travis-ci.org/news-r/textanalysis.svg?branch=master)](https://travis-ci.org/news-r/textanalysis) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) <!-- badges: end -->

textanalysis
============

Text Analysis in R via Julia.

Installation
------------

Being a wrapper to a [Julia](https://julialang.org/) package, textanalysis requires the latter to be installed.

``` r
# install.packages("remotes")
remotes::install_github("news-r/textanalysis") # github
```

Setup
-----

You *must* run `init_textanalysis` at the begining of every session, you will otherwise encounter errors and be prompted to do so.

``` r
textanalysis::init_textanalysis() # setup word2vec Julia dependency
```

Example
-------

``` r
library(textanalysis)

# build document
doc <- string_document("They <span>write</span>, it writes too!!!")

# basic cleanup
prepare(doc)
#> ⚠ This function changes `document` in place!
get_text(doc)
#> [1] "  write     writes  "

# stem
stem_words(doc)
get_text(doc)
#> [1] "write write"

# corpus
doc2 <- token_document("Hey write another document.")
get_text(doc2)
#> [1] "Hey write another document ."

# combine
corpus <- corpus(doc, doc2)

# standardize
standardize(corpus, "token_document")
#> ⚠ This function changes `corpus` in place!

# prepare corpus
corpus <- prepare(corpus, strip_html_tags = FALSE)
#> ⚠ This function changes `cropus` in place!
get_text(corpus[[1]])
#> [1] "write write"

update_lexicon(corpus)
lexicon(corpus)
#> # A tibble: 4 x 2
#>   words        n
#>   <chr>    <int>
#> 1 document     1
#> 2 write        3
#> 3 hey          1
#> 4 ""           2
```
