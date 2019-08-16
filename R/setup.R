#' Initialise Session
#' 
#' Initialise a session, installs TextAnalysis Juia dependency if needed.
#' 
#' @param ... Arguments passed to \link[JuliaCall]{julia_setup}.
#' @param version Whether to install the stable version from the 
#' registry (recommended) or the latest version from github.
#' 
#' @details Some functions currently require the \code{latest} Github version.
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
init_textanalysis <- function(..., version = c("stable", "latest")){
  julia <- JuliaCall::julia_setup(...)
  version <- match.arg(version)
  pkg <- "TextAnalysis"
  if(version == "latest") pkg <- "https://github.com/JuliaText/TextAnalysis.jl"
  julia_install_package_if_needed(pkg)
  julia_install_package_if_needed("Languages")
  julia_library("TextAnalysis")
  cat(crayon::green(cli::symbol$tick), "textanalysis initialised.\n")
}

#' @rdname init
#' @export
setup_textanalysis <- init_textanalysis

#' @rdname init
#' @export
install_textanalysis <- function(version = c("stable", "latest")){
  version <- match.arg(version)
  pkg <- "TextAnalysis"
  if(version == "latest") pkg <- "https://github.com/JuliaText/TextAnalysis.jl"
  JuliaCall::julia_install_package(pkg)
}