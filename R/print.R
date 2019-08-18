#' @export
print.documents <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info), 
    length(x),
    "documents.\n",
    ...
  )
}

#' @export
print.document <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info), 
    "A single document.\n",
    ...
  )
}

#' @export
print.corpus <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info), 
    "A corpus of",
    length(x), 
    "documents.\n",
    ...
  )
}