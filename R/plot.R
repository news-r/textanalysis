#' @export
plot.coom <- function(x, y, ...){
  ggcorrplot::ggcorrplot(x, ...)
}