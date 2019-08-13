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
  julia_call("FileDocument", path)
}

#' @rdname documents
#' @export 
string_document <- function(text) {
  assert_that(!missing(text), msg = "Missing `text`")
  assert_that(length(text) == 1)
  julia_call("StringDocument", text)
}

#' @rdname documents
#' @export
token_document <- function(text) {
  assert_that(!missing(text), msg = "Missing `text`")
  assert_that(length(text) == 1)
  julia_call("TokenDocument", text)
}

#' @rdname documents
#' @export 
ngram_document <- function(text, ...) {
  assert_that(!missing(text), msg = "Missing `text`")
  assert_that(length(text) == 1)
  julia_call("NGramDocument", text, ...)
}