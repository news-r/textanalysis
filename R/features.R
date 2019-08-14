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