call_julia <- function(func, ...) {
  assert_that(has_ta(func))
  julia_call(func, ...)
}