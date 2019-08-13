#' Preprocess Document
#' 
#' Preprocess the document, note that this replaces the object in place.
#' 
#' @inheritParams get_text
#' @param remove_corrupt_utf8 Remove corrupt UTF8 characters.
#' @param remove_case Convert to lowercase.
#' @param strip_punctuation Remove punctuation.
#' @param strip_stopwords Remove stopwords, i.e.: "all", "almost", "alone".
#' @param strip_numbers Remove numbers.
#' @param strip_non_letters Remove anything non-numeric.
#' @param strip_html_tags Remove html tags, including the style and script tags.
#' @param remove_words Remove the occurences of words from `doc`.
#' @param strip_spares_terms Remove sparse terms.
#' @param strip_frequent_terms Remove frequent terms.
#' @param strip_articles Remove articles: "a", "an", "the".
#' @param strip_indefinite_articles Removes indefinite articles:  "a", "an".
#' @param strip_definite_articles Remove "the".
#' @param strip_preposition Remove preprositions, i.e.: "across", "around", "before".
#' @param strip_pronouns Remove pronounces, i.e.: "I", "you", "he", "she".
#' @param ... Other special classes
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("This <span>is</span> a very short document!!!")
#' 
#' # replaces in place!
#' prepare_document(doc)
#' get_text(doc)
#' }
#' 
#' @seealso \code{\link{stem_document}} to stem your document.
#' 
#' @name prepare_document
#' @export
prepare_document <- function(document, ...) UseMethod("prepare_document")

#' @rdname prepare_document
#' @method prepare_document document
#' @export
prepare_document.document <- function(document, remove_corrupt_utf8 = TRUE, remove_case = TRUE, strip_stopwords = TRUE, 
  strip_numbers = TRUE, strip_html_tags = TRUE, strip_punctuation = TRUE, remove_words = NULL, strip_non_letters = FALSE, 
  strip_spares_terms = FALSE, strip_frequent_terms = FALSE, strip_articles = FALSE, 
  strip_indefinite_articles = FALSE, strip_definite_articles = FALSE, strip_preposition = FALSE, 
  strip_pronouns = FALSE, ...){

  cat(
    crayon::yellow(cli::symbol$warning),
    "This function replaces `document`",
    crayon::yellow("in place"), "\n"
  )
  
  # warn as docs advises otherwise
  validate_that(has_corrupt(remove_corrupt_utf8))

  if(remove_corrupt_utf8) call_julia("remove_corrupt_utf8!", document)
  if(remove_case) call_julia("remove_case!", document)
  if(!is.null(remove_words)) call_julia("remove_words!", document, remove_words)

  # use prepare function
  classes <- c(...)
  if(strip_articles) classes <- append(classes, "strip_articles")
  if(strip_indefinite_articles) classes <- append(classes, "strip_indefinite_articles")
  if(strip_definite_articles) classes <- append(classes, "strip_definite_articles")
  if(strip_preposition) classes <- append(classes, "strip_preposition")
  if(strip_pronouns) classes <- append(classes, "strip_pronouns")
  if(strip_stopwords) classes <- append(classes, "strip_stopwords")
  if(strip_non_letters) classes <- append(classes, "strip_non_letters")
  if(strip_spares_terms) classes <- append(classes, "strip_spares_terms")
  if(strip_frequent_terms) classes <- append(classes, "strip_frequent_terms")
  if(strip_html_tags) classes <- append(classes, "strip_html_tags")
  if(strip_punctuation) classes <- append(classes, "strip_punctuation")
  
  # return early if nothing to strip
  if(!length(classes))
    return(document)

  # keep that to re-assign later
  keep_class <- class(document)
  julia_assign("sd", document)

  expr <- paste0(classes, collapse = "| ")

  julia_eval(paste0("prepare!(sd, ", expr, ")"))

  document <- julia_eval("sd")
  document <- structure(document, class = keep_class)
  invisible(document)
}

#' Stem
#' 
#' Stem document
#' 
#' @inheritParams get_text
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("They write, it writes")
#' 
#' # replaces in place!
#' stem_document(doc)
#' get_text(doc)
#' }
#' 
#' @name stem_document
#' @export
stem_document <- function(document) UseMethod("stem_document")

#' @rdname stem_document
#' @method stem_document document
#' @export
stem_document.document <- function(document){
  call_julia("stem!", document)
  invisible()
}
