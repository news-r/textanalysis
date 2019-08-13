#' Preprocess Document
#' 
#' Preprocess the document, note that this replaces the object in place.
#' 
#' @param text An object inheriting of class \code{document} or \code{corpus}.
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
#' prepare(doc)
#' get_text(doc)
#' }
#' 
#' @seealso \code{\link{stem_words}} to stem your document.
#' 
#' @name prepare
#' @export
prepare <- function(text, ...) UseMethod("prepare")

#' @rdname prepare
#' @method prepare document
#' @export
prepare.document <- function(text, remove_corrupt_utf8 = TRUE, remove_case = TRUE, strip_stopwords = TRUE, 
  strip_numbers = TRUE, strip_html_tags = TRUE, strip_punctuation = TRUE, remove_words = NULL, strip_non_letters = FALSE, 
  strip_spares_terms = FALSE, strip_frequent_terms = FALSE, strip_articles = FALSE, 
  strip_indefinite_articles = FALSE, strip_definite_articles = FALSE, strip_preposition = FALSE, 
  strip_pronouns = FALSE, ...){

  warning_in_place("document")
  
  # warn as docs advises otherwise
  validate_that(has_corrupt(remove_corrupt_utf8))

  if(remove_corrupt_utf8) call_julia("remove_corrupt_utf8!", text)
  if(remove_case) call_julia("remove_case!", text)
  if(!is.null(remove_words)) call_julia("remove_words!", text, remove_words)

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
    return(text)

  # keep that to re-assign later
  keep_class <- class(text)
  julia_assign("sd", text)

  expr <- paste0(classes, collapse = "| ")

  julia_eval(paste0("prepare!(sd, ", expr, ")"))

  text <- julia_eval("sd")
  text <- structure(text, class = keep_class)
  invisible(text)
}

#' @rdname prepare
#' @method prepare corpus
#' @export
prepare.corpus <- function(text, remove_corrupt_utf8 = TRUE, remove_case = TRUE, strip_stopwords = TRUE, 
  strip_numbers = TRUE, strip_html_tags = TRUE, strip_punctuation = TRUE, remove_words = NULL, strip_non_letters = FALSE, 
  strip_spares_terms = FALSE, strip_frequent_terms = FALSE, strip_articles = FALSE, 
  strip_indefinite_articles = FALSE, strip_definite_articles = FALSE, strip_preposition = FALSE, 
  strip_pronouns = FALSE, ...){

  warning_in_place("cropus")
  
  # warn as docs advises otherwise
  validate_that(has_corrupt(remove_corrupt_utf8))

  if(remove_corrupt_utf8) call_julia("remove_corrupt_utf8!", text)
  if(remove_case) call_julia("remove_case!", text)
  if(!is.null(remove_words)) call_julia("remove_words!", text, remove_words)

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
    return(text)

  # keep that to re-assign later
  keep_class <- class(text)
  julia_assign("sd", text)

  expr <- paste0(classes, collapse = "| ")

  julia_eval(paste0("prepare!(sd, ", expr, ")"))

  text <- julia_eval("sd")
  text <- structure(text, class = keep_class)
  invisible(text)
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
#' stem_words(doc)
#' get_text(doc)
#' }
#' 
#' @name stem_words
#' @export
stem_words <- function(document) UseMethod("stem_words")

#' @rdname stem_words
#' @method stem_words document
#' @export
stem_words.document <- function(document){
  call_julia("stem!", document)
  invisible()
}
