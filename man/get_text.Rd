% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document.R
\name{get_text}
\alias{get_text}
\alias{get_text.document}
\alias{get_text.documents}
\alias{get_text.corpus}
\title{Extract Text}
\usage{
get_text(document)

\method{get_text}{document}(document)

\method{get_text}{documents}(document)

\method{get_text}{corpus}(document)
}
\arguments{
\item{document}{A document as returned by the \code{*_document} 
family of functions, i.e.: \code{\link{string_document}}.}
}
\description{
Access the text of documents as a string.
}
\examples{
\dontrun{
init_textanalysis()

# build document
doc <- string_document("This is a document.")

# extract text
get_text(doc)
}

}
