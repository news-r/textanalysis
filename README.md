
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
textanalysis::init_textanalysis() # setup textanalysis Julia dependency
```

Example
-------

Very basic.

``` r
library(textanalysis)

# build document
str <- paste(
  "They <span>write</span>, it writes too!!!",
  "This is another sentence.",
  "More stuff in this document."
)
doc <- string_document(str)

# basic cleanup
prepare(doc)
#> ⚠ This function changes `document` in place!
get_text(doc)
#> [1] "  write     writes         sentence    stuff     document"

# stem
stem_words(doc)
get_text(doc)
#> [1] "write write sentenc stuff document"

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
prepare(corpus, strip_html_tags = FALSE)
#> ⚠ This function changes `cropus` in place!
get_text(corpus[[1]])
#> [1] "write write sentenc stuff document"

# lexicon + lexical stats
update_lexicon(corpus)
lexicon(corpus)
#> # A tibble: 6 x 2
#>   words        n
#>   <chr>    <int>
#> 1 stuff        1
#> 2 document     2
#> 3 write        3
#> 4 hey          1
#> 5 ""           2
#> 6 sentenc      1
lexical_frequency(corpus, "document")
#> [1] 0.2

# inverse index
update_inverse_index(corpus)
inverse_index(corpus)
#> [1] 6

# dtm
m <- document_term_matrix(corpus)

# term-frequency
tf(m)
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]  0.0  0.2  0.0  0.2  0.2  0.4
#> [2,]  0.4  0.2  0.2  0.0  0.0  0.2

# tf-idf
tf_idf(m)
#>           [,1] [,2]      [,3]      [,4]      [,5] [,6]
#> [1,] 0.0000000    0 0.0000000 0.1386294 0.1386294    0
#> [2,] 0.2772589    0 0.1386294 0.0000000 0.0000000    0

# sentiment
sentiment(corpus)
#> [1] 0.5834179 0.5475631

# summarise in 2 sentences
summarize(string_document(str), ns = 2L)
#> [1] "They <span>write</span>, it writes too!!!"
#> [2] "This is another sentence."
```

fit LDA on the [gensimr](https://gensimr.news-r.org) data.

``` r
set.seed(42)

data("corpus", package = "gensimr")
documents <- as_documents(corpus) # convert vector to documents

crps <- corpus(documents)
update_lexicon(crps)
dtm <- document_term_matrix(crps)

# 50 topics
# 10K iterations
lda_data <- lda(dtm, 2L, 1000L)

# visualise topics
plot(t(lda_data$ntopics_ndocs))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r

mat <- dtm_matrix(dtm, "dense")

tfidf <- tf_idf(mat)

km <- kmeans(tfidf, centers = 2)

km_data <- dplyr::bind_cols(as.data.frame(t(lda_data$ntopics_ndocs)), tibble::tibble(cluster = km$cluster))

plot(jitter(km_data$V1), jitter(km_data$V2), col = km_data$cluster)
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />
