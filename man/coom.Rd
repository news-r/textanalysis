% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features.R
\name{coom}
\alias{coom}
\alias{coom.corpus}
\title{Co-occurrence Matrix}
\usage{
coom(corpus, window = 5L, normalize = TRUE)

\method{coom}{corpus}(corpus, window = 5L, normalize = TRUE)
}
\arguments{
\item{corpus}{A \code{corpus} as returned by \code{\link{corpus}}.}

\item{window}{Size of the sliding word window.}

\item{normalize}{Whether to normalize the counts by the 
distance between word positions.}
}
\description{
The elements of the Co occurrence matrix indicate how many 
times two words co-occur in a (sliding) word window of a given size.
}
\section{Plot}{

The plot method returns an object of class \code{echarts4r}.
}

\examples{
\dontrun{
init_textanalysis()

# create corpus
doc <- string_document("A simple document.")
doc2 <- string_document("Another simple document.")
crps <- corpus(doc, doc2)

# matrix & plot
matrix <- coom(crps)
plot(matrix)
}

}
