% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{remove_words}
\alias{remove_words}
\alias{remove_words.corpus}
\alias{remove_words.documents}
\alias{remove_words.document}
\title{Remove Specific Words}
\usage{
remove_words(text, words)

\method{remove_words}{corpus}(text, words)

\method{remove_words}{documents}(text, words)

\method{remove_words}{document}(text, words)
}
\arguments{
\item{text}{An object inheriting of class \code{document} or \code{corpus}.}

\item{words}{Word to remove.}
}
\description{
Remove specific words.
}
\examples{
\dontrun{
init_textanalysis()

# build document
doc <- string_document("this woops is not woop correct")

# replaces in place!
errors <- c("woops", "woop")
remove_words(doc, words = errors)
get_text(doc)
}

}
