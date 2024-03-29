% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpus.R
\name{corpus_metadata}
\alias{corpus_metadata}
\alias{titles_}
\alias{titles_.corpus}
\alias{titles_.JuliaObject}
\alias{languages_}
\alias{languages_.corpus}
\alias{languages_.JuliaObject}
\alias{authors_}
\alias{authors_.corpus}
\alias{authors_.JuliaObject}
\alias{timestamps_}
\alias{timestamps_.corpus}
\alias{timestamps_.JuliaObject}
\title{Corpus Metadata}
\usage{
titles_(corpus, ...)

\method{titles_}{corpus}(corpus, ...)

\method{titles_}{JuliaObject}(corpus, ...)

languages_(corpus, ...)

\method{languages_}{corpus}(corpus, ...)

\method{languages_}{JuliaObject}(corpus, ...)

authors_(corpus, ...)

\method{authors_}{corpus}(corpus, ...)

\method{authors_}{JuliaObject}(corpus, ...)

timestamps_(corpus, ...)

\method{timestamps_}{corpus}(corpus, ...)

\method{timestamps_}{JuliaObject}(corpus, ...)
}
\arguments{
\item{corpus}{A corpus, as returned vy \code{\link{corpus}}.}

\item{...}{A character string to set.}
}
\description{
Get and set corpus metadata.
}
\details{
Note that the value (three dots) for the \code{language}
function is taken from the Julia \code{Language} package.
}
\examples{
\dontrun{
init_textanalysis()

# build document
doc1 <- string_document("First document.")
doc2 <- string_document("Second document.")

corpus <- corpus(doc1, doc2)

# get and et metadata
titles_(corpus) # get
titles_(corpus, list("Hello", "World")) # set
}

}
