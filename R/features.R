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
  julia_eval(expr)
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
  assert_that(!missing(document), msg = "Missing `document`")
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
  call_julia("tf", dtm)
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
  call_julia("tf_idf", dtm)
}

#' @rdname tf_idf
#' @method tf_idf JuliaObject
#' @export
tf_idf.JuliaObject <- tf_idf.dtm