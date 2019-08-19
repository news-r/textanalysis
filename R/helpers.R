#' Set Seed in Julia
#' 
#' Set a seed in Julia, similar to \code{set.seed} in R.
#' 
#' @param x Seed, an integer.
#' 
#' @examples
#' \dontrun{set_seed(42)}
#' 
#' @export
set_seed <- function(x) {
  assert_that(is_missing(x))
  x <- as.integer(x)
  julia_library("Random")
  expr <- paste0("Random.seed!(", x,")")
  julia_command(expr)
}