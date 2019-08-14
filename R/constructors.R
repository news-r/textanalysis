.construct_document <- function(x, ...) {
  structure(x, class = c("document", ..., class(x)))
}

.construct_corpus <- function(x, ...) {
  structure(x, class = c("corpus", ..., class(x)))
}

.construct_dtm <- function(x, ...) {
  structure(x, class = c("dtm", ..., class(x)))
}
