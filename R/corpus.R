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


#' Corpus Metadata
#' 
#' Get and set corpus metadata.
#' 
#' @inheritParams standardize
#' @param ... A character string to set.
#' 
#' @details Note that the value (three dots) for the \code{language}
#' function is taken from the Julia \code{Language} package.
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
#' 
#' # get and et metadata
#' titles_(corpus) # get
#' titles_(corpus, list("Hello", "World")) # set
#' }
#' 
#' @name corpus_metadata
#' @export
titles_ <- function(corpus, ...) UseMethod("titles_")

#' @rdname corpus_metadata
#' @method titles_ corpus
#' @export
titles_.corpus <- function(corpus, ...){
  L <- length(list(...))
  func <- ifelse(L > 0, "title!", "title")
  call_julia(func, corpus, ...)
}

#' @rdname corpus_metadata
#' @method titles_ JuliaObject
#' @export
titles_.JuliaObject <- titles_.corpus

#' @rdname corpus_metadata
#' @export
languages_ <- function(corpus, ...) UseMethod("languages_")

#' @rdname corpus_metadata
#' @method languages_ corpus
#' @export
languages_.corpus <- function(corpus, ...){
  L <- length(list(...))
  if(L == 0){
    call_julia("language", corpus)
  } else {
    lang <- list(...)[[1]] # get language
    assert_that(has_ta(lang)) # test if Language pkg installed & lang valid
    julia_assign("sd", corpus)
    expr <- paste0("language!(sd, Languages.", lang,"())")
    julia_eval(expr)
  }
}

#' @rdname corpus_metadata
#' @method languages_ JuliaObject
#' @export
languages_.JuliaObject <- languages_.corpus

#' @rdname corpus_metadata
#' @export
authors_ <- function(corpus, ...) UseMethod("authors_")

#' @rdname corpus_metadata
#' @method authors_ corpus
#' @export
authors_.corpus <- function(corpus, ...){
  L <- length(list(...))
  func <- ifelse(L > 0, "author!", "author")
  call_julia(func, corpus, ...)
}

#' @rdname corpus_metadata
#' @method authors_ JuliaObject
#' @export
authors_.JuliaObject <- authors_.corpus

#' @rdname corpus_metadata
#' @export
timestamps_ <- function(corpus, ...) UseMethod("timestamp_")

#' @rdname corpus_metadata
#' @method timestamps_ corpus
#' @export
timestamps_.corpus <- function(corpus, ...){
  L <- length(list(...))
  func <- ifelse(L > 0, "timestamp!", "timestamp")
  call_julia(func, corpus, ...)
}

#' @rdname corpus_metadata
#' @method timestamps_ JuliaObject
#' @export
timestamps_.JuliaObject <- timestamps_.corpus

#' Lexicon
#' 
#' The lexicon of a corpus consists of all the terms that 
#' occur in any document in the corpus. The lexical frequency 
#' of a term tells us how often a term occurs across all of the 
#' documents. Often the most interesting words in a document 
#' are those words whose frequency within a document is higher 
#' than their frequency in the corpus as a whole.
#'
#' @inheritParams standardize
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
#' 
#' update_lexicon(corpus)
#' lexicon(corpus)
#' }
#' 
#' @name lexicon
#' @export
lexicon <- function(corpus) UseMethod("lexicon")

#' @rdname lexicon
#' @export
update_lexicon <- function(corpus) UseMethod("update_lexicon")

#' @rdname lexicon
#' @method lexicon corpus
#' @export
lexicon.corpus <- function(corpus) {
  lex <- call_julia("lexicon", corpus)
  if(!length(lex))
    stop("No lexicon, see `update_lexicon`")
  
  tibble::tibble(
    words = names(lex),
    n = unname(unlist(lex))
  )
}

#' @rdname lexicon
#' @method update_lexicon corpus
#' @export
update_lexicon.corpus <- function(corpus) {
  call_julia("update_lexicon!", corpus)
  invisible()
}

#' Lexical Frequency
#'
#' How often a term occurs across all of the documents.
#' 
#' @inheritParams standardize
#' @param word Word to check frequency.
#' 
#' @name lexical_frequency 
#' @export
lexical_frequency <- function(corpus, word) UseMethod("lexical_frequency")

#' @rdname lexical_frequency
#' @method lexical_frequency corpus
#' @export
lexical_frequency.corpus <- function(corpus, word){
  assert_that(!missing(word), msg = "Missing `word`")
  call_julia("lexical_frequency", corpus, word)
}