% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpus.R
\name{standardize}
\alias{standardize}
\alias{standardize.corpus}
\title{Standardize}
\usage{
standardize(corpus, type = c("string_document", "file_document",
  "token_document", "ngram_document"))

\method{standardize}{corpus}(corpus, type = c("string_document",
  "file_document", "token_document", "ngram_document"))
}
\arguments{
\item{corpus}{A corpus, as returned vy \code{\link{corpus}}.}

\item{type}{Type to convert to.}
}
\description{
Standardize documents in a corpus.
}
\examples{
\dontrun{
init_textanalysis()

# build document
doc1 <- string_document("First document.")
doc2 <- token_document("Second document.")

corpus <- corpus(doc1, doc2)
standardize(corpus)
}

}
