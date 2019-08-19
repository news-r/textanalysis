#' Random Projection Model
#'
#' In mathematics and statistics, random projection 
#' is a technique used to reduce the dimensionality 
#' of a set of points which lie in Euclidean space. 
#' Random projection methods are powerful methods 
#' known for their simplicity and less erroneous 
#' output compared with other methods. According to 
#' experimental results, random projection preserve 
#' distances well, but empirical results are sparse. 
#' They have been applied to many natural language 
#' tasks under the name of random indexing. The core 
#' idea behind random projection is given in the 
#' Johnson-Lindenstrauss lemma which states that if 
#' points in a vector space are of sufficiently high 
#' dimension, then they may be projected into a suitable 
#' lower-dimensional space in a way which approximately 
#' preserves the distances between the points.
#' 
#' @param text An object inheriting of class \code{document} or \code{corpus}.
#' @param ... Any other options to pass to the model 
#' \url{https://zgornel.github.io/StringAnalysis.jl/dev/examples/#Dimensionality-reduction-1}.
#' 
#' @examples
#' \dontrun{
#' # Use stringanalysis backend!
#' init_stringanalysis()
#' 
#' # build document
#' doc1 <- string_document("First document.")
#' doc2 <- string_document("Second document.")
#' 
#' crps <- corpus(doc1, doc2)
#' dtm <- document_term_matrix(crps)
#' model <- rp_model(dtm)
#' }
#' 
#' @name rp_model
rp_model <- function(text, ...) UseMethod("rp_model")

#' @rdname rp_model
#' @method rp_model dtm
rp_model.dtm <- function(text, ...){
  assert_that(has_sa())
  call_julia("RPModel", text, ...)
}

#' @rdname rp_model
rp_model.corpus <- function(text, ...){
  assert_that(has_sa())
  call_julia("rp", text, ...)
}