#' DocumentTermMatrix
#' 
#' Represent documents as a matrix of word counts.
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
#' m <- document_term_matrix(corpus)
#' }
#' 
#' @name document_term_matrix 
#' @export
document_term_matrix <- function(corpus) UseMethod("document_term_matrix")

#' @rdname document_term_matrix 
#' @method document_term_matrix corpus
#' @export
document_term_matrix.corpus <- function(corpus){
  dtm <- call_julia("DocumentTermMatrix", corpus)
  .construct_dtm(dtm)
}

#' Sparse Matrix
#' 
#' Creates a simple sparse matrix of \code{document_term_matrix} object.
#' 
#' @param dtm An object of class \code{document_term_matrix} 
#' as returned by \code{document_term_matrix}.
#' @param density Whether to return a \code{dense} or \code{sparse} matrix.
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
#' m <- document_term_matrix(corpus)
#' dtm_matrix(m) # sparse
#' dtm_matrix(m, "dense") # dense
#' }
#' 
#' @name dtm_matrix
#' @export
dtm_matrix <- function(dtm, density) UseMethod("dtm_matrix")

#' @rdname dtm_matrix
#' @method dtm_matrix dtm
#' @export
dtm_matrix.dtm <- function(dtm, density = c("sparse", "dense")){
  density <- match.arg(density)
  density <- paste0(":", density)
  julia_assign("m", dtm)
  expr <- paste0("dtm(m, ", density, ")")
  dtm <- julia_eval(expr)
  .construct_dtm(dtm)
}

#' Document Term Vector
#' 
#' Produce a single row of a DocumentTermMatrix.
#' 
#' @inheritParams standardize
#' @param document Index of document within the corpus.
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
#' document_term_vector(corpus)
#' }
#' 
#' @name document_term_vector
#' @export
document_term_vector <- function(corpus, document) UseMethod("document_term_vector")

#' @rdname document_term_vector
#' @method document_term_vector corpus
#' @export
document_term_vector.corpus <- function(corpus, document){
  assert_that(is_missing(document))
  assert_that(is.numeric(document))

  julia_assign("crps", corpus)
  expr <- paste0("dtv(crps[", document, "], lexicon(crps))")
  julia_eval(expr)
}

#' Term Frequency
#' 
#' Compute term-frequency.
#' 
#' @inheritParams dtm_matrix
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
#' m <- document_term_matrix(corpus)
#' tf(m)
#' }
#' 
#' @name tf
#' @export
tf <- function(dtm) UseMethod("tf")

#' @rdname tf
#' @method tf dtm
#' @export
tf.dtm <- function(dtm){
  call_julia("tf", dtm) %>% t()
}

#' @rdname tf
#' @method tf JuliaObject
#' @export
tf.JuliaObject <- tf.dtm

#' Term Frequency Inverse Document Freqency
#' 
#' Compute term-frequency inverse-document-frequency.
#' 
#' @inheritParams dtm_matrix
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
#' m <- document_term_matrix(corpus)
#' tf_idf(m)
#' }
#' 
#' @name tf_idf
#' @export
tf_idf <- function(dtm) UseMethod("tf_idf")

#' @rdname tf_idf
#' @method tf_idf dtm
#' @export
tf_idf.dtm <- function(dtm){
  call_julia("tf_idf", dtm) %>% t()
}

#' @rdname tf_idf
#' @method tf_idf JuliaObject
#' @export
tf_idf.JuliaObject <- tf_idf.dtm

#' Sentiment Analyzer
#' 
#' Find the sentiment score (between 0 and 1) of a 
#' word, sentence or a Document based on a trained 
#' model (using Flux) on IMDB word corpus with weights 
#' saved are used to calculate the sentiments.
#' 
#' @param text An object of class \code{document} or \code{corpus}.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document(
#'   "An awesome, great, simply brillaint, function!"
#' )
#' 
#' sentiment(doc)
#' }
#' 
#' @name sentiment
#' @export
sentiment <- function(text) UseMethod("sentiment")

#' @rdname sentiment
#' @method sentiment document
#' @export
sentiment.document <- function(text){
  if(!julia_exists("textanalysisSentiment"))
    julia_eval("textanalysisSentiment = SentimentAnalyzer()")
  call_julia("textanalysisSentiment", text)
}

#' @rdname sentiment
#' @method sentiment documents
#' @export
sentiment.documents <- function(text){
  if(!julia_exists("textanalysisSentiment"))
    julia_eval("textanalysisSentiment = SentimentAnalyzer()")
  purrr::map(text, sentiment)
}

#' @rdname sentiment
#' @method sentiment corpus
#' @export
sentiment.corpus <- function(text){
  if(!julia_exists("textanalysisSentiment"))
    julia_eval("textanalysisSentiment = SentimentAnalyzer()")
  L <- length(text)
  scores <- c()
  for(i in seq_along(1:L)){
    score <- call_julia("textanalysisSentiment", text[[i]])
    scores <- append(scores, score)
  }
  return(scores)
}


#' Summarize
#' 
#' Simple text-rank based summarizer.
#' 
#' @inheritParams sentiment
#' @param ns Number of sentences.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc <- string_document(
#'   paste("Assume this Short Document as an example.",
#'     "Assume this as an example summarizer.", 
#'     "This has too few sentences."
#'   )
#' )
#' 
#' summarizer(doc, ns = 2L)
#' }
#' 
#' @name summarizer
#' @export
summarizer <- function(text, ns = 2L) UseMethod("summarizer")

#' @rdname summarizer
#' @method summarizer document
#' @export
summarizer.document <- function(text, ns = 2L){
  call_julia("summarize", text, ns = ns)
}

#' @rdname summarizer
#' @method summarizer corpus
#' @export
summarizer.corpus <- function(text, ns = 2L){
  L <- length(text)
  summarizations <- c()
  for(i in seq_along(1:L)){
    summ <- call_julia("summarize", text[[i]], ns = ns)
    summarizations <- append(summarizations, summ)
  }
  return(summarizations)
}

#' Hash Trick
#' 
#' Replace terms with their hashed valued using a hash 
#' function that outputs integers from 1 to N.
#' 
#' @param cardinality Max index used for hashing (default 100).
#' @param text A \code{corpus}, \code{character} string, a \code{document},
#' or a \code{document_term_matrix}.
#' @param hash_func A hash function as returned by \code{create_hash_function}.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' hash_func <- create_hash_function(10L)
#' hash("a", hash_func)
#' 
#' doc <- string_document("A simple document.")
#' hash(doc, hash_func)
#' }
#' 
#' @name create_hash_function
#' @export
create_hash_function <- function(cardinality = 100L){
  assert_that(is_missing(cardinality))
  cardinality <- as.integer(cardinality)
  func <- call_julia("TextHashFunction", cardinality)
  .construct_hash_function(func)
}

#' @rdname create_hash_function
#' @export
hash <- function(text, hash_func) UseMethod("hash")

#' @rdname create_hash_function
#' @method hash character
#' @export
hash.character <- function(text, hash_func){
  assert_that(is_missing(hash_func))

  purrr::map(text, function(x, hash_func){
    call_julia("index_hash", x, hash_func)
  }, hash_func = hash_func)  %>% 
    unlist()
}

#' @rdname create_hash_function
#' @method hash document
#' @export
hash.document <- function(text, hash_func = NULL){
  assert_that(is_missing(hash_func))

  if(!is.null(hash_func))
    call_julia("hash_dtv", text, hash_func)
  else
    call_julia("hash_dtv", text)
}

#' @rdname create_hash_function
#' @method hash documents
#' @export
hash.documents <- function(text, hash_func = NULL){
  assert_that(is_missing(hash_func))

  purrr::map(text, hash, hash_func) 
}

#' @rdname create_hash_function
#' @method hash dtm
#' @export
hash.dtm <- function(text, hash_func = NULL){
  assert_that(is_missing(hash_func))

  if(!is.null(hash_func))
    call_julia("hash_dtm", text, hash_func)
  else
    call_julia("hash_dtm", text)
}

#' @rdname create_hash_function
#' @method hash corpus
#' @export
hash.corpus <- function(text, hash_func = NULL){
  assert_that(is_missing(hash_func))

  if(!is.null(hash_func))
    call_julia("hash_dtm", text, hash_func)
  else
    call_julia("hash_dtm", text)
}

#' Co-occurrence Matrix
#' 
#' The elements of the Co occurrence matrix indicate how many 
#' times two words co-occur in a (sliding) word window of a given size. 
#' 
#' @param corpus A \code{corpus} as returned by \code{\link{corpus}}.
#' @param window Size of the sliding word window.
#' @param normalize Whether to normalize the counts by the 
#' distance between word positions.
#' 
#' @section Plot:
#' The plot method returns an object of class \code{echarts4r}.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # create corpus
#' doc <- string_document("A simple document.")
#' doc2 <- string_document("Another simple document.")
#' crps <- corpus(doc, doc2)
#' 
#' # matrix & plot
#' matrix <- coom(crps)
#' plot(matrix)
#' }
#' 
#' @name coom
#' @export
coom <- function(corpus, window = 5L, normalize = TRUE) UseMethod("coom")

#' @rdname coom
#' @method coom corpus
#' @export
coom.corpus <- function(corpus, window = 5L, normalize = TRUE){
  julia_assign("crps", corpus)
  expr <- paste0('CooMatrix(crps, window=', window, ', normalize=', tolower(normalize),')')
  coom <- julia_eval(expr)
  matrix <- call_julia("coom", coom)
  colnames(matrix) <- coom$terms
  row.names(matrix) <- coom$terms
  .construct_coom(matrix)
}


#' Okapi BM-25
#'
#' Okapi BM25 (BM stands for Best Matching) is a ranking 
#' function used by search engines to estimate the relevance 
#' of documents to a given search query. 
#'
#' @param text A document-term martrix or a \code{corpus}.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # create corpus
#' doc <- string_document("A simple document.")
#' doc2 <- string_document("Another simple document.")
#' crps <- corpus(doc, doc2)
#' 
#' # matrix & plot
#' bm_25(crps)
#' }
#' 
#' @name bm_25
#' @export
bm_25 <- function(text) UseMethod("bm_25")

#' @rdname bm_25
#' @method bm_25 dtm
#' @export
bm_25.dtm <- function(text){
  call_julia("bm_25", text) %>% t()
}

#' @rdname bm_25
#' @method bm_25 corpus
#' @export
bm_25.corpus <- function(text){
  dtm <- document_term_matrix(text)
  call_julia("bm_25", text) %>% t()
}
