# initialised textanalysis?
has_ta <- function(x) {
  julia_exists(x)
}

on_failure(has_ta) <- function(call, env) {
  paste0(
    "Dependency ", 
    crayon::red("not found"),
    " see `",
    crayon::blue("init_textanalysis"),
    "`",
    " or `",
    crayon::blue("init_stringanalysis"),
    "`."
  )
}

# initialised stringanalysis?
has_sa <- function(x) {
  julia_exists(x)
}

on_failure(has_sa) <- function(call, env) {
  paste0(
    "Dependency ", 
    crayon::red("not found"),
    " see `",
    crayon::blue("init_stringanalysis"),
    "`"
  )
}

# corrupt?
has_corrupt <- function(x) {
  isTRUE(x)
}

on_failure(has_corrupt) <- function(call, env) {
  paste0(
    "May contain ", 
    crayon::yellow("corrupt utf8"),
    " characters: set `",
    crayon::blue("remove_corrupt_utf8"),
    "` to `",
    crayon::blue("TRUE"),
    "`"
  )
}

# missing var pretty print
is_missing <- function(x) {
  !missing(x)
}

on_failure(is_missing) <- function(call, env) {
  paste0(
    "Missing `",
    crayon::blue(call$x),
    "`."
  )
}