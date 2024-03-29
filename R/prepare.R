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
#' @param strip_sparse_terms Remove sparse terms.
#' @param strip_frequent_terms Remove frequent terms.
#' @param strip_articles Remove articles: "a", "an", "the".
#' @param strip_indefinite_articles Removes indefinite articles:  "a", "an".
#' @param strip_definite_articles Remove "the".
#' @param strip_preposition Remove preprositions, i.e.: "across", "around", "before".
#' @param strip_pronouns Remove pronounces, i.e.: "I", "you", "he", "she".
#' @param ... Other special classes
#' @param update_lexicon Whether to update the lexicon of the corpus, 
#' see \code{\link{update_lexicon}}.
#' @param update_inverse_index Whether to update the inverse index of the corpus, 
#' see \code{\link{update_inverse_index}}.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("This <span>is</span> a very short document!.!")
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
  strip_sparse_terms = FALSE, strip_frequent_terms = FALSE, strip_articles = FALSE, 
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
  if(strip_numbers) classes <- append(classes, "strip_numbers")
  if(strip_non_letters) classes <- append(classes, "strip_non_letters")
  if(strip_sparse_terms) classes <- append(classes, "strip_sparse_terms")
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
#' @method prepare documents
#' @export
prepare.documents <- function(text, remove_corrupt_utf8 = TRUE, remove_case = TRUE, strip_stopwords = TRUE, 
  strip_numbers = TRUE, strip_html_tags = TRUE, strip_punctuation = TRUE, remove_words = NULL, strip_non_letters = FALSE, 
  strip_sparse_terms = FALSE, strip_frequent_terms = FALSE, strip_articles = FALSE, 
  strip_indefinite_articles = FALSE, strip_definite_articles = FALSE, strip_preposition = FALSE, 
  strip_pronouns = FALSE, ...){

  warning_in_place("documents")

  quiet_prepare <- purrr::quietly(prepare)

  purrr::map(text, quiet_prepare,  remove_corrupt_utf8 = remove_corrupt_utf8, remove_case = remove_case, strip_stopwords = strip_stopwords, 
    strip_numbers = strip_numbers, strip_html_tags = strip_html_tags, strip_punctuation = strip_punctuation, remove_words = remove_words,
    strip_non_letters = strip_non_letters, strip_sparse_terms = strip_sparse_terms, strip_frequent_terms = strip_frequent_terms, 
    strip_articles = strip_articles, strip_indefinite_articles = strip_indefinite_articles, strip_definite_articles = strip_definite_articles,
    strip_preposition = strip_preposition, strip_pronouns = strip_pronouns, ...)
  invisible()
}

#' @rdname prepare
#' @method prepare corpus
#' @export
prepare.corpus <- function(text, remove_corrupt_utf8 = TRUE, remove_case = TRUE, strip_stopwords = TRUE, 
  strip_numbers = TRUE, strip_html_tags = TRUE, strip_punctuation = TRUE, remove_words = NULL, strip_non_letters = FALSE, 
  strip_sparse_terms = FALSE, strip_frequent_terms = FALSE, strip_articles = FALSE, 
  strip_indefinite_articles = FALSE, strip_definite_articles = FALSE, strip_preposition = FALSE, 
  strip_pronouns = FALSE, ..., update_lexicon = TRUE, update_inverse_index = TRUE){

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
  if(strip_numbers) classes <- append(classes, "strip_numbers")
  if(strip_sparse_terms) classes <- append(classes, "strip_sparse_terms")
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
  if(update_lexicon) update_lexicon(text)
  if(update_inverse_index) update_inverse_index(text)
  invisible(text)
}

#' Remove Upper case
#' 
#' Turn everything to lowercase.
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("ThIs DoCuMENT HAs UppERCase")
#' 
#' # replaces in place!
#' remove_case(doc)
#' get_text(doc)
#' }
#' 
#' @name remove_case
#' @export
remove_case <- function(text) UseMethod("remove_case")

#' @rdname remove_case
#' @method remove_case corpus
#' @export
remove_case.corpus <- function(text){
  call_julia("remove_case!", text)
  invisible()
} 

#' @rdname remove_case
#' @method remove_case documents
#' @export
remove_case.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname remove_case
#' @method remove_case document
#' @export
remove_case.document <- remove_case.corpus

#' Strip Articles
#' 
#' Remove articles, or the words "a", "an", "the".
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("This document has an article.")
#' 
#' # replaces in place!
#' strip_articles(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_articles
#' @export
strip_articles <- function(text) UseMethod("strip_articles")

#' @rdname strip_articles
#' @method strip_articles corpus
#' @export
strip_articles.corpus <- function(text){
  .strip("articles", text)
  invisible()
} 

#' @rdname strip_articles
#' @method strip_articles documents
#' @export
strip_articles.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_articles
#' @method strip_articles document
#' @export
strip_articles.document <- strip_articles.corpus

#' Strip Indefinite Articles
#' 
#' Remove articles, or the words "a", "an".
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("This is an indifite article.")
#' 
#' # replaces in place!
#' strip_indefinite_articles(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_indefinite_articles
#' @export
strip_indefinite_articles <- function(text) UseMethod("strip_indefinite_articles")

#' @rdname strip_indefinite_articles
#' @method strip_indefinite_articles corpus
#' @export
strip_indefinite_articles.corpus <- function(text){
  .strip("indefinite_articles", text)
  invisible()
} 

#' @rdname strip_indefinite_articles
#' @method strip_indefinite_articles documents
#' @export
strip_indefinite_articles.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_indefinite_articles
#' @method strip_indefinite_articles document
#' @export
strip_indefinite_articles.document <- strip_indefinite_articles.corpus

#' Remove Corrupt UTF8
#' 
#' Remove corrupt UTF8 characters that might cause issues, recommended.
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("this document is clean")
#' 
#' # replaces in place!
#' remove_corrupt_utf8(doc)
#' }
#' 
#' @name remove_corrupt_utf8
#' @export
remove_corrupt_utf8 <- function(text) UseMethod("remove_corrupt_utf8")

#' @rdname remove_corrupt_utf8
#' @method remove_corrupt_utf8 corpus
#' @export
remove_corrupt_utf8.corpus <- function(text){
  call_julia("remove_corrupt_utf8!", text)
  invisible()
} 

#' @rdname remove_corrupt_utf8
#' @method remove_corrupt_utf8 documents
#' @export
remove_corrupt_utf8.documents <- function(text){
  purrr::map(text, remove_corrupt_utf8)
  invisible()
} 

#' @rdname remove_corrupt_utf8
#' @method remove_corrupt_utf8 document
#' @export
remove_corrupt_utf8.document <- remove_corrupt_utf8.corpus

#' Remove Specific Words
#' 
#' Remove specific words.
#' 
#' @inheritParams prepare
#' @param words Word to remove.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("this woops is not woop correct")
#' 
#' # replaces in place!
#' errors <- c("woops", "woop")
#' remove_words(doc, words = errors)
#' get_text(doc)
#' }
#' 
#' @name remove_words
#' @export
remove_words <- function(text, words) UseMethod("remove_words")

#' @rdname remove_words
#' @method remove_words corpus
#' @export
remove_words.corpus <- function(text, words){
  call_julia("remove_words!", text, words)
  invisible()
} 

#' @rdname remove_words
#' @method remove_words documents
#' @export
remove_words.documents <- function(text, words){
  purrr::map(text, remove_words, words)
  invisible()
} 

#' @rdname remove_words
#' @method remove_words document
#' @export
remove_words.document <- remove_words.corpus

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

#' Strip Definite Articles
#' 
#' Remove articles, or the word "the".
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("This document has the article.")
#' 
#' # replaces in place!
#' strip_definite_articles(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_definite_articles
#' @export
strip_definite_articles <- function(text) UseMethod("strip_definite_articles")

#' @rdname strip_definite_articles
#' @method strip_definite_articles corpus
#' @export
strip_definite_articles.corpus <- function(text){
  .strip("definite_articles", text)
  invisible()
} 

#' @rdname strip_definite_articles
#' @method strip_definite_articles documents
#' @export
strip_definite_articles.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_definite_articles
#' @method strip_definite_articles document
#' @export
strip_definite_articles.document <- strip_definite_articles.corpus

#' Strip Preprositions
#' 
#' Remove preprositions, or the words such as "across", "around", "before" (and many more).
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document("A preprosition is in the document.")
#' 
#' # replaces in place!
#' strip_preprositions(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_preprositions
#' @export
strip_preprositions <- function(text) UseMethod("strip_preprositions")

#' @rdname strip_preprositions
#' @method strip_preprositions corpus
#' @export
strip_preprositions.corpus <- function(text){
  .strip("prepositions", text)
  invisible()
} 

#' @rdname strip_preprositions
#' @method strip_preprositions documents
#' @export
strip_preprositions.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_preprositions
#' @method strip_preprositions document
#' @export
strip_preprositions.document <- strip_preprositions.corpus

#' Strip Pronouns
#' 
#' Remove pronouns, or the words such as "I", "you", "he", "she", (and many more).
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' # must be lowercase
#' doc <- string_document("she is the pronoun.")
#' 
#' # replaces in place!
#' strip_pronouns(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_pronouns
#' @export
strip_pronouns <- function(text) UseMethod("strip_pronouns")

#' @rdname strip_pronouns
#' @method strip_pronouns corpus
#' @export
strip_pronouns.corpus <- function(text){
  .strip("pronouns", text)
  invisible()
} 

#' @rdname strip_pronouns
#' @method strip_pronouns documents
#' @export
strip_pronouns.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_pronouns
#' @method strip_pronouns document
#' @export
strip_pronouns.document <- strip_pronouns.corpus

#' Strip Stopwords
#' 
#' Remove stopwords, or the words such as "all", "almost", "alone", (and many more).
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' # must be lowercase
#' doc <- string_document("The document has a stop word.")
#' 
#' # replaces in place!
#' strip_stopwords(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_stopwords
#' @export
strip_stopwords <- function(text) UseMethod("strip_stopwords")

#' @rdname strip_stopwords
#' @method strip_stopwords corpus
#' @export
strip_stopwords.corpus <- function(text){
  .strip("stopwords", text)
  invisible()
} 

#' @rdname strip_stopwords
#' @method strip_stopwords documents
#' @export
strip_stopwords.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_stopwords
#' @method strip_stopwords document
#' @export
strip_stopwords.document <- strip_stopwords.corpus

#' Strip Stopwords
#' 
#' Remove stopwords, or the words such as "all", "almost", "alone", (and many more).
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' # must be lowercase
#' doc <- string_document("This is 1 document.")
#' 
#' # replaces in place!
#' strip_non_letters(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_non_letters
#' @export
strip_non_letters <- function(text) UseMethod("strip_non_letters")

#' @rdname strip_non_letters
#' @method strip_non_letters corpus
#' @export
strip_non_letters.corpus <- function(text){
  .strip("non_letters", text)
  invisible()
} 

#' @rdname strip_non_letters
#' @method strip_non_letters documents
#' @export
strip_non_letters.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_non_letters
#' @method strip_non_letters document
#' @export
strip_non_letters.document <- strip_non_letters.corpus

#' Strip Numbers
#' 
#' Remove numbers.
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' # must be lowercase
#' doc <- string_document("This is 1 document.")
#' 
#' # replaces in place!
#' strip_numbers(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_numbers
#' @export
strip_numbers <- function(text) UseMethod("strip_numbers")

#' @rdname strip_numbers
#' @method strip_numbers corpus
#' @export
strip_numbers.corpus <- function(text){
  .strip("numbers", text)
  invisible()
} 

#' @rdname strip_numbers
#' @method strip_numbers documents
#' @export
strip_numbers.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_numbers
#' @method strip_numbers document
#' @export
strip_numbers.document <- strip_numbers.corpus

#' Strip Sparse Terms
#' 
#' Remove sparse words.
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' # must be lowercase
#' doc <- string_document("This is 1 document.")
#' 
#' # replaces in place!
#' strip_sparse_terms(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_sparse_terms
#' @export
strip_sparse_terms <- function(text) UseMethod("strip_sparse_terms")

#' @rdname strip_sparse_terms
#' @method strip_sparse_terms corpus
#' @export
strip_sparse_terms.corpus <- function(text){
  .strip("sparse_terms", text)
  invisible()
} 

#' @rdname strip_sparse_terms
#' @method strip_sparse_terms documents
#' @export
strip_sparse_terms.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_sparse_terms
#' @method strip_sparse_terms document
#' @export
strip_sparse_terms.document <- strip_sparse_terms.corpus

#' Strip Sparse Terms
#' 
#' Remove sparse words.
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' str <- paste0(
#'   "This is a sentence.",
#'   "This is another sentence."
#' )
#' doc <- string_document(str)
#' 
#' # replaces in place!
#' strip_sparse_terms(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_sparse_terms
#' @export
strip_sparse_terms <- function(text) UseMethod("strip_sparse_terms")

#' @rdname strip_sparse_terms
#' @method strip_sparse_terms corpus
#' @export
strip_sparse_terms.corpus <- function(text){
  .strip("sparse_terms", text)
  invisible()
} 

#' @rdname strip_sparse_terms
#' @method strip_sparse_terms documents
#' @export
strip_sparse_terms.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_sparse_terms
#' @method strip_sparse_terms document
#' @export
strip_sparse_terms.document <- strip_sparse_terms.corpus

#' Strip Frequent Terms
#' 
#' Remove frequently occuring words.
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' # must be lowercase
#' doc <- string_document("This is 1 document.")
#' 
#' # replaces in place!
#' strip_frequent_terms(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_frequent_terms
#' @export
strip_frequent_terms <- function(text) UseMethod("strip_frequent_terms")

#' @rdname strip_frequent_terms
#' @method strip_frequent_terms corpus
#' @export
strip_frequent_terms.corpus <- function(text){
  .strip("frequent_terms", text)
  invisible()
} 

#' @rdname strip_frequent_terms
#' @method strip_frequent_terms documents
#' @export
strip_frequent_terms.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_frequent_terms
#' @method strip_frequent_terms document
#' @export
strip_frequent_terms.document <- strip_frequent_terms.corpus

#' Strip HTML Tags
#' 
#' Remove html tags.
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' # must be lowercase
#' doc <- string_document("This is a <span>document</span>.")
#' 
#' # replaces in place!
#' strip_html_tags(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_html_tags
#' @export
strip_html_tags <- function(text) UseMethod("strip_html_tags")

#' @rdname strip_html_tags
#' @method strip_html_tags corpus
#' @export
strip_html_tags.corpus <- function(text){
  .strip("html_tags", text)
  invisible()
} 

#' @rdname strip_html_tags
#' @method strip_html_tags documents
#' @export
strip_html_tags.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_html_tags
#' @method strip_html_tags document
#' @export
strip_html_tags.document <- strip_html_tags.corpus

#' Strip Punctuation
#' 
#' Remove punctuation marks.
#' 
#' @inheritParams prepare
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' # must be lowercase
#' doc <- string_document(".This is a document!!")
#' 
#' # replaces in place!
#' strip_punctuation(doc)
#' get_text(doc)
#' }
#' 
#' @name strip_punctuation
#' @export
strip_punctuation <- function(text) UseMethod("strip_punctuation")

#' @rdname strip_punctuation
#' @method strip_punctuation corpus
#' @export
strip_punctuation.corpus <- function(text){
  .strip("punctuation", text)
  invisible()
} 

#' @rdname strip_punctuation
#' @method strip_punctuation documents
#' @export
strip_punctuation.documents <- function(text){
  purrr::map(text, remove_case)
  invisible()
} 

#' @rdname strip_punctuation
#' @method strip_punctuation document
#' @export
strip_punctuation.document <- strip_punctuation.corpus
