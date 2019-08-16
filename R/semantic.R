#' Latent Dirichlet Analysis
#' 
#' Perform Latent Dirichlet Analysis or lda on a term-document matrix.
#' 
#' @inheritParams dtm_matrix
#' @param topics,iter Number of topics and iterations.
#' @param alpha Dirichlet dist. hyperparameter for topic distribution per document.
#' \code{alpha < 1} yields a sparse topic mixture for each document. 
#' \code{alpha > 1} yields a more uniform topic mixture for each document.
#' @param beta Dirichlet dist. hyperparameter for word distribution per topic. 
#' \code{beta < 1} yields a sparse word mixture for each topic.
#' \code{beta > 1} yields a more uniform word mixture for each topic.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc1 <- string_document("First document. Another sentence")
#' doc2 <- string_document("Some example written here.")
#' doc3 <- string_document("This is a string document")
#' doc4 <- string_document("Yet another document for the corpus.")
#' 
#' crps <- corpus(doc1, doc2, doc3, doc4)
#' 
#' update_lexicon(crps)
#' 
#' m <- document_term_matrix(crps)
#' lda <- lda(m, 2L, 1000L, .1, .1)
#' }
#' 
#' @return A list containing.
#' \itemize{
#'  \item{\code{ntopics_nwords} \code{ntopics * nwords} Sparse matrix of probabilities s.t. \eqn{sum(ntopics_nwords, 1) == 1}.}
#'  \item{\code{ntopics_ndocs} \code{ntopics * ndocs} Dense matrix of probabilities s.t. \eqn{sum(θ, 1) == 1}.}
#' }
#' 
#' @name lda
#' @export
lda <- function(dtm, topics = 2L, iter = 1000L, alpha = .1, beta = .1) UseMethod("lda")

#' @rdname lda
#' @method lda dtm
#' @export
lda.dtm <- function(dtm, topics = 2L, iter = 1000L, alpha = .1, beta = .1){
  topics <- as.integer(topics)
  iter <- as.integer(iter)
  lda <- call_julia("lda", dtm, topics, iter, alpha, beta)
  list(
    ntopics_nwords = lda[[1]],
    ntopics_ndocs = lda[[2]]
  )
}

#' Latent Semantic Analysis
#' 
#' Perform Latent Semantic Analysis or lda on a corpus.
#' 
#' @inheritParams standardize
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' 
#' # build document
#' doc1 <- string_document("First document. Another sentence")
#' doc2 <- string_document("Some example written here.")
#' doc3 <- string_document("This is a string document")
#' doc4 <- string_document("Yet another document for the corpus.")
#' 
#' crps <- corpus(doc1, doc2, doc3, doc4)
#' 
#' lsa(crps)
#' }
#' 
#' @name lsa
#' @export
lsa <- function(corpus) UseMethod("lsa")

#' @rdname lsa
#' @method lsa corpus
#' @export
lsa.corpus <- function(corpus){
  cat(crayon::yellow(cli::symbol$warning), "\n")
  lsa <- call_julia("lsa", corpus)
}

#' @rdname lsa
#' @method lsa dtm
#' @export
lsa.dtm <- lsa.corpus