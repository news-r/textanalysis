#' @export
print.documents <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info), 
    length(x),
    "documents. \n",
    ...
  )
}

#' @export
print.corpus <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info), 
    length(x),
    "documents in corpus. \n",
    ...
  )
}