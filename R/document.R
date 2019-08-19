#' Document
#' 
#' The basic unit of text analysis is a document. 
#' The textanalysis package allows one to work with documents 
#' stored in a variety of formats.
#' 
#' @param path The path to the file.
#' @param text The text as a character string, or tokens, or ngrams as a list.
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
#' @return An object of class \code{document}.
#' 
#' @seealso \code{\link{directory_corpus}} to read a dicrectory of files as corpus, 
#' and \code{\link{to_documents}} to parse a vector or data.frame to documents.
#' 
#' @name documents
#' @export 
file_document <- function(path) {
  assert_that(is_missing(path))
  assert_that(file.exists(path), msg = "File does not exist.")
  assert_that(length(path) == 1)

  # Julia requires  normalized path
  path <- normalizePath(path)
  doc <- call_julia("FileDocument", path)
  .construct_document(doc)
}

#' @rdname documents
#' @export 
string_document <- function(text) {
  assert_that(is_missing(text))
  assert_that(length(text) == 1)
  doc <- call_julia("StringDocument", text)
  .construct_document(doc, "string_document")
}

#' @rdname documents
#' @export
token_document <- function(text) {
  assert_that(is_missing(text))
  assert_that(length(text) == 1)
  doc <- call_julia("TokenDocument", text)
  .construct_document(doc, "token_document")
}

#' @rdname documents
#' @export 
ngram_document <- function(text, ...) {
  assert_that(is_missing(text))
  assert_that(length(text) == 1)
  doc <- call_julia("NGramDocument", text, ...)
  .construct_document(doc, "ngram_document")
}

#' Create Multiple Documents
#' 
#' Create multiple documents from a \code{data.frame}, 
#' or \code{vector} of characters.
#'
#' @param documents A \code{data.frame}, or \code{vector} 
#' containing documents.
#' @param text Bare column name containing data.
#' @param type Type of \code{*_document} function to use, see \code{\link{documents}}.
#' @param title,language,author,timestamp Bare column name 
#' to metadata to add to documents, see \code{\link{document_metadata}} .
#' @param ... Not actually being used.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' docs <- c(
#'   "This is a document.",
#'   "This is another document."
#' )
#' to_documents(docs)
#' 
#' docs_df <- tibble::tibble(
#'   txt = docs,
#'   title = c("A", "B") 
#' )
#' to_documents(docs_df)
#' title_(docs_df)
#' }
#'
#' @seealso \code{\link{documents}} to read a single document.
#' 
#' @return An object of class \code{documents}.
#' 
#' @name to_documents
#' @export 
to_documents <- function(documents, ...) UseMethod("to_documents")

#' @rdname to_documents
#' @method to_documents character
#' @export 
to_documents.character <- function(documents, ..., type = c("string", "token", "ngram")){
  assert_that(
    length(documents) > 1, 
    msg = "User other `*_document` functions for a single document."
  )
  type <- match.arg(type)

  documents <- purrr::map(documents, .doc_by_type, type = type)

  if(type == "ngram")
    .construct_documents(documents, "ngram_documents")
  else
    .construct_documents(documents)
}

#' @rdname to_documents
#' @method to_documents data.frame
#' @export 
to_documents.data.frame <- function(documents, ..., text, title = NULL, 
  language = NULL, author = NULL, timestamp = NULL, 
  type = c("string", "token", "ngram")){
  assert_that(is_missing(text))
  type <- match.arg(type)

  text_quo   <- dplyr::enquo(text)
  title_quo  <- dplyr::enquo(title)
  lang_quo   <- dplyr::enquo(language)
  author_quo <- dplyr::enquo(author)
  ts_quo     <- dplyr::enquo(timestamp)

  documents <- documents %>% 
    dplyr::select(
      text      = !!text_quo,
      title     = !!title_quo,
      language  = !!lang_quo,
      author    = !!author_quo,
      timestamp = !!ts_quo
    ) %>% 
    as.data.frame()

  vars <- names(documents)

  new_docs <- list() 
  for(i in 1:nrow(documents)){
    # force character conversion as factor fails
    txt <- as.character(documents$text[i])
    doc <- .doc_by_type(txt, type)

    if("title" %in% vars && !is.na(documents[i, "title"])) 
      title_(doc, documents[i, "title"])
    if("language" %in% vars && !is.na(documents[i, "language"])) 
      language_(doc, documents[i, "language"])
    if("author" %in% vars && !is.na(documents[i, "author"])) 
      author_(doc, documents[i, "author"])
    if("timestamp" %in% vars && !is.na(documents[i, "timestamp"])) 
      timestamp_(doc, documents[i, "timestamp"])

    new_docs <- append(new_docs, doc)
  }

  if(type == "ngram")
    .construct_documents(new_docs, "ngram_documents")
  else
    .construct_documents(new_docs)
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
#' @method get_text documents
#' @export
get_text.documents <- function(document){
  text <- purrr::map(document, get_text) %>% 
    unlist()
  tibble::tibble(
    text = text,
    document = seq_along(1:length(text))
  )
}

#' @rdname get_text
#' @method get_text corpus
#' @export
get_text.corpus <- function(document){
  text <- tibble::tibble()
  for(i in 1:length(document)){
    txt <- call_julia("text", document[[i]])
    txt <- tibble::tibble(
      text = txt,
      document = i
    )
    text <- dplyr::bind_rows(text, txt)
  }
  return(text)
}

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
#' @method get_tokens documents
#' @export
get_tokens.documents <- function(document){
  text <- tibble::tibble()
  for(i in 1:length(document)){
    txt <- call_julia("tokens", document[[i]])
    tib <- tibble::tibble(
      tokens = txt,
      document = i
    )
    text <- dplyr::bind_rows(text, tib)
  }
  return(text)
}

#' @rdname get_tokens
#' @method get_tokens corpus
#' @export
get_tokens.corpus <- function(document){
  text <- tibble::tibble()
  for(i in 1:length(document)){
    txt <- call_julia("tokens", document[[i]])
    tib <- tibble::tibble(
      tokens = txt,
      document = i
    )
    text <- dplyr::bind_rows(text, tib)
  }
  return(text)
}

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
#' @method get_ngrams documents
#' @export
get_ngrams.documents <- function(document, ...){
  purrr::map_dfr(document, get_ngrams, ...) 
}

#' @rdname get_ngrams
#' @method get_ngrams corpus
#' @export
get_ngrams.corpus <- function(document, ...){
  text <- tibble::tibble()
  for(i in 1:length(document)){
    x <- call_julia("ngrams", document[[i]], ...)
    txt <- tibble::tibble(
      document = i,
      ngrams = names(x),
      n = unname(unlist(x))
    )
    text <- dplyr::bind_rows(text, txt)
  }
  return(text)
}

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
#' @method ngram_complexity ngram_documents
#' @export
ngram_complexity.ngram_documents <- function(document){
  purrr::map(document, ngram_complexity) %>% 
    unlist()
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
#' @method title_ documents
#' @export
title_.documents <- function(document, ...){
  purrr::map(document, title_, ...) %>% unlist()
}

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
#' @method language_ documents
#' @export
language_.documents <- function(document, ...){
  purrr::map(document, language_, ...) %>% unlist()
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
#' @method author_ documents
#' @export
author_.documents <- function(document, ...){
  purrr::map(document, author_, ...) %>% unlist()
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
#' @method timestamp_ documents
#' @export
timestamp_.documents <- function(document, ...){
  purrr::map(document, timestamp_, ...) %>% unlist()
}

#' @rdname document_metadata
#' @method timestamp_ JuliaObject
#' @export
timestamp_.JuliaObject <- timestamp_.document
