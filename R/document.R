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
  .construct_document(doc)
}

#' @rdname documents
#' @export
token_document <- function(text) {
  assert_that(!missing(text), msg = "Missing `text`")
  assert_that(length(text) == 1)
  doc <- call_julia("TokenDocument", text)
  doc <- .construct_document(doc)
}

#' @rdname documents
#' @export 
ngram_document <- function(text, ...) {
  assert_that(!missing(text), msg = "Missing `text`")
  assert_that(length(text) == 1)
  doc <- call_julia("NGramDocument", text, ...)
  .construct_document(doc)
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

#' Extract Tokens
#' 
#' Access tokens of documents as a string.
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
#' # extract text
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