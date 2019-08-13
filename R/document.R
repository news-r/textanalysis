#' Document
#' 
#' The basic unit of text analysis is a document. 
#' The textanalysis package allows one to work with documents 
#' stored in a variety of formats.
#' 
#' @param path The path to the file.
#' @param text The text, tokens, or ngrams.
#' @param ... Other positonal arguments.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' doc <- "This is a document."
#' string_document(doc)
#' ngram_document(doc, 2L)
#' }
#' 
#' @name documents
#' @export 
file_document <- function(path) {
  assert_that(!missing(path), msg = "Missing `path`")
  assert_that(file.exists(path), msg = "File does not exist")
  assert_that(length(path) == 1)

  # Julia requires  normalized path
  path <- normalizePath(path)
  doc <- call_julia("FileDocument", path)
  .construct_document(doc)
}

#' @rdname documents
#' @export 
string_document <- function(text) {
  assert_that(!missing(text), msg = "Missing `text`")
  assert_that(length(text) == 1)
  doc <- call_julia("StringDocument", text)
  .construct_document(doc, "string_document")
}

#' @rdname documents
#' @export
token_document <- function(text) {
  assert_that(!missing(text), msg = "Missing `text`")
  assert_that(length(text) == 1)
  doc <- call_julia("TokenDocument", text)
  .construct_document(doc, "token_document")
}

#' @rdname documents
#' @export 
ngram_document <- function(text, ...) {
  assert_that(!missing(text), msg = "Missing `text`")
  assert_that(length(text) == 1)
  doc <- call_julia("NGramDocument", text, ...)
  .construct_document(doc, "ngram_document")
}

#' Extract Text
#' 
#' Access the text of documents as a string.
#' 
#' @param document A document as returned by the \code{*_document} 
#' family of functions, i.e.: \code{\link{string_document}}.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("This is a document.")
#' 
#' # extract text
#' get_text(doc)
#' }
#' 
#' @name get_text
#' @export
get_text <- function(document) UseMethod("get_text")

#' @rdname get_text
#' @method get_text document
#' @export
get_text.document <- function(document){
  call_julia("text", document)
}

#' @rdname get_text
#' @method get_text JuliaObject
#' @export
get_text.JuliaObject <- get_text.document 

#' Extract Tokens
#' 
#' Access tokens of documents as a vector.
#' 
#' @inheritParams get_text
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("This is a document.")
#' 
#' # extract tokens
#' get_tokens(doc)
#' }
#' 
#' @name get_tokens
#' @export
get_tokens <- function(document) UseMethod("get_tokens")

#' @rdname get_tokens
#' @method get_tokens document
#' @export
get_tokens.document <- function(document){
  call_julia("tokens", document)
}

#' @rdname get_tokens
#' @method get_tokens JuliaObject
#' @export
get_tokens.JuliaObject <- get_tokens.document 

#' Extract NGrams
#' 
#' Access n-grams tokens of documents as a vector.
#' 
#' @inheritParams get_text
#' @param ... Any other positional arguments.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("This is a document.")
#' 
#' # extract n-grams
#' get_ngrams(doc)
#' get_ngrams(doc, 2L)
#' }
#' 
#' @return A \link[tibble]{tibble} of ngrams and their occurences.
#' 
#' @name get_ngrams
#' @export
get_ngrams <- function(document, ...) UseMethod("get_ngrams")

#' @rdname get_ngrams
#' @method get_ngrams document
#' @export
get_ngrams.document <- function(document, ...){
  x <- call_julia("ngrams", document, ...)
  tibble::tibble(
    ngrams = names(x),
    n = unname(unlist(x))
  )
}

#' @rdname get_ngrams
#' @method get_ngrams JuliaObject
#' @export
get_ngrams.JuliaObject <- get_ngrams.document

#' Determine NGram Complexity
#' 
#' Determine whether an \code{ngram_document} (output of \code{\link{ngram_document}}) 
#' contains unigrams, bigrams or a higher-order representation.
#' 
#' @inheritParams get_text
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' unigram <- ngram_document("This is a document.", 1L)
#' bigram <- ngram_document("This is a document.", 2L)
#' 
#' # test complexity
#' identical(ngram_complexity(unigram), 1L)
#' identical(ngram_complexity(bigram), 2L)
#' }
#' 
#' @name ngram_complexity
#' @export
ngram_complexity <- function(document) UseMethod("ngram_complexity")

#' @rdname ngram_complexity
#' @method ngram_complexity ngram_document
#' @export
ngram_complexity.ngram_document <- function(document){
  call_julia("ngram_complexity", document)
}

#' @rdname ngram_complexity
#' @method ngram_complexity JuliaObject
#' @export
ngram_complexity.JuliaObject <- ngram_complexity.ngram_document

#' Document Metadata
#' 
#' Get and set document metadata.
#' 
#' @inheritParams get_text
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
#' doc <- string_document("This is a document.")
#' 
#' # get and et metadata
#' title_(doc) # get
#' title_(doc, "Hello World") # set
#' language_(doc, "Spanish") # from Language pack
#' }
#' 
#' @name document_metadata
#' @export
title_ <- function(document, ...) UseMethod("title_")

#' @rdname document_metadata
#' @method title_ document
#' @export
title_.document <- function(document, ...){
  L <- length(list(...))
  func <- ifelse(L > 0, "title!", "title")
  call_julia(func, document, ...)
}

#' @rdname document_metadata
#' @method title_ JuliaObject
#' @export
title_.JuliaObject <- title_.document

#' @rdname document_metadata
#' @export
language_ <- function(document, ...) UseMethod("language_")

#' @rdname document_metadata
#' @method language_ document
#' @export
language_.document <- function(document, ...){
  L <- length(list(...))
  if(L == 0){
    call_julia("language", document)
  } else {
    lang <- list(...)[[1]] # get language
    assert_that(has_ta(lang)) # test if Language pkg installed & lang valid
    julia_assign("sd", document)
    expr <- paste0("language!(sd, Languages.", lang,"())")
    julia_eval(expr)
  }
}

#' @rdname document_metadata
#' @method language_ JuliaObject
#' @export
language_.JuliaObject <- language_.document

#' @rdname document_metadata
#' @export
author_ <- function(document, ...) UseMethod("author_")

#' @rdname document_metadata
#' @method author_ document
#' @export
author_.document <- function(document, ...){
  L <- length(list(...))
  func <- ifelse(L > 0, "author!", "author")
  call_julia(func, document, ...)
}

#' @rdname document_metadata
#' @method author_ JuliaObject
#' @export
author_.JuliaObject <- author_.document

#' @rdname document_metadata
#' @export
timestamp_ <- function(document, ...) UseMethod("timestamp_")

#' @rdname document_metadata
#' @method timestamp_ document
#' @export
timestamp_.document <- function(document, ...){
  L <- length(list(...))
  func <- ifelse(L > 0, "timestamp!", "timestamp")
  call_julia(func, document, ...)
}

#' @rdname document_metadata
#' @method timestamp_ JuliaObject
#' @export
timestamp_.JuliaObject <- timestamp_.document