.construct_document <- function(x, ...) {
  structure(x, class = c("document", ..., class(x)))
}

.construct_documents <- function(x, ...) {
  x %>% 
    purrr::map(.construct_document) %>% 
    structure(class = c("documents", ..., class(.)))
}

.construct_corpus <- function(x, ...) {
  structure(x, class = c("corpus", ..., class(x)))
}

.construct_dtm <- function(x, ...) {
  structure(x, class = c("document_term_matrix", "dtm", ..., class(x)))
}

.construct_naive_bayes <- function(x, ...) {
  structure(x, class = c("naive_bayes_model", ..., class(x)))
}

.construct_hash_function <- function(x, ...) {
  structure(x, class = c("hash_function", ..., class(x)))
}

.construct_coom <- function(x, ...) {
  structure(x, class = c("co_occurence_matrix", "coom", ..., class(x)))
}