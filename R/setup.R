#' Initialise Session
#' 
#' Initialise a session, installs TextAnalysis Juia dependency if needed.
#' 
#' @param ... Arguments passed to \link[JuliaCall]{julia_setup}.
#' @param type Whether to install the stable version from the 
#' registry (recommended) or the latest version from github.
#' 
#' @section Packages:
#' Packages installed by \code{init_textanalysis} are:
#' \itemize{
#'   \item{\code{TextAnalysis}}
#'   \item{\code{Languages}}
#' }
#' 
#' @import JuliaCall
#' @import assertthat
#' 
#' @name init
#' @export
init_textanalysis <- function(..., type = c("stable", "latest")){
  julia <- JuliaCall::julia_setup(...)
  type <- match.arg(type)
  pkg <- "TextAnalysis"
  if(type == "latest") pkg <- "https://github.com/JuliaText/TextAnalysis.jl"
  JuliaCall::julia_install_package_if_needed(pkg)
  JuliaCall::julia_install_package_if_needed("Languages")
  JuliaCall::julia_library("TextAnalysis")
  cat(crayon::green(cli::symbol$tick), "textanalysis initialised.\n")
}

#' @rdname init
#' @export
setup_textanalysis <- init_textanalysis