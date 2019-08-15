.construct_document <- function(x, ...) {
  structure(x, class = c("document", ..., class(x)))
}

.construct_documents <- function(x, ...) {
  x %>% 
    purrr::map(.construct_document) %>% 
    structure(class = c("documents", ..., class(x)))
}

.construct_corpus <- function(x, ...) {
  structure(x, class = c("corpus", ..., class(x)))
}

.construct_dtm <- function(x, ...) {
  structure(x, class = c("document_term_matrix", "dtm", ..., class(x)))
}
