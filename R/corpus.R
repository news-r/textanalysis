#' Corpus
#' 
#' Build a corpus of documents.
#' 
#' @param ... Documents.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc1 <- string_document("First document.")
#' doc2 <- string_document("Second document.")
#' 
#' corpus <- corpus(doc1, doc2)
#' }
#' 
#' @name corpus
#' 
#' @export
corpus <- function(...) {
  corpus <- call_julia("Corpus", JuliaObject(list(...)))
  .construct_corpus(corpus)
}

#' Standardize
#' 
#' Standardize documents in a corpus.
#' 
#' @param corpus A corpus, as returned vy \code{\link{corpus}}.
#' @param type Type to convert to.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc1 <- string_document("First document.")
#' doc2 <- token_document("Second document.")
#' 
#' corpus <- corpus(doc1, doc2)
#' standardize(corpus)
#' }
#' 
#' @name standardize
#' @export
standardize <- function(corpus, type = c("string_document", "file_document", "token_document", "ngram_document")) UseMethod("standardize")

#' @rdname standardize
#' @method standardize corpus
#' @export
standardize.corpus <- function(corpus, type = c("string_document", "file_document", "token_document", "ngram_document")){
  # check
  type <- match.arg(type)
  warning_in_place("corpus")

  # build type
  type <- .build_type(type)

  # call julia
  julia_assign("crps", corpus)
  expr <- paste0("standardize!(crps, ", type, ")")
  julia_eval(expr)

  # fetch and rebuild
  corpus <- julia_eval("crps")
  corpus <- .construct_corpus(corpus)

  invisible(corpus)
}