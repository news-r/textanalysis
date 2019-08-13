has_ta <- function(x) {
  julia_exists(x)
}

on_failure(has_ta) <- function(call, env) {
  paste0(
    "Dependency ", 
    crayon::red("not found"),
    " see `",
    crayon::blue("init_textanalysis"),
    "`"
  )
}