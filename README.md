
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
[![Travis build status](https://travis-ci.org/news-r/textanalysis.svg?branch=master)](https://travis-ci.org/news-r/textanalysis) <!-- badges: end -->

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
#> Julia version 1.1.1 at location /home/jp/Downloads/julia-1.1.1-linux-x86_64/julia-1.1.1/bin will be used.
#> Loading setup script for JuliaCall...
#> Finish loading setup script for JuliaCall.
```

Example
-------

``` r
library(textanalysis)

# build document
doc <- string_document("They <span>write</span>, it writes too!!!")

# basic cleanup
prepare_document(doc)
#> ⚠ This function replaces `document` in place
get_text(doc)
#> [1] "  write     writes  "

# stem
stem_document(doc)
get_text(doc)
#> [1] "write write"
```
