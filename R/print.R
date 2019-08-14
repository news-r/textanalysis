#' @export
print.documents <- function(x, ...){
  cat(
    crayon::blue(cli::symbol$info), 
    length(x),
    "documents. \n",
    ...
  )
}