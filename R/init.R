#' Initialise Session
#' 
#' Initialise a session, installs TextAnalysis or StringAnalysis
#' Juia dependency if needed.
#' 
#' @param ... Arguments passed to \link[JuliaCall]{julia_setup}.
#' @param version Whether to install the stable version from the 
#' registry (recommended) or the latest version from github.
#' 
#' @details Some functions might require the \code{latest} Github version.
#' 
#' @section Backends:
#' Two backends are available, 
#' \href{https://github.com/JuliaText/TextAnalysis.jl}{TextAnalysis} and 
#' \href{https://github.com/zgornel/StringAnalysis.jl}{StringAnalysis}. 
#' The former is the original repository while the second is a hard-fork,
#' of the former which is "designed to provide a richer, faster and orthogonal API."
#' 
#' @section Packages:
#' Packages installed by \code{init_textanalysis} are:
#' \itemize{
#'   \item{\code{TextAnalysis}}
#'   \item{\code{Languages}}
#' }
#' 
#' Packages installed by \code{init_stringanalysis} are:
#' \itemize{
#'   \item{\code{StringAnalysis}}
#'   \item{\code{Languages}}
#' }
#' 
#' @return Invisibly returns \code{TRUE} is successful.
#' 
#' @import JuliaCall
#' @import assertthat
#' 
#' @name init
#' @export
init_textanalysis <- function(...){
  julia <- julia_setup(...)
  julia_install_package_if_needed("TextAnalysis")
  julia_install_package_if_needed("Languages")
  julia_library("TextAnalysis")
  cat(crayon::green(cli::symbol$tick), "textanalysis backend initialised.\n")
  invisible(TRUE)
}

#' @rdname init
#' @export
init_stringanalysis <- function(...){
  julia <- julia_setup(...)
  julia_install_package_if_needed("StringAnalysis")
  julia_install_package_if_needed("Languages")
  julia_library("StringAnalysis")
  cat(crayon::green(cli::symbol$tick), "stringanalysis backend initialised.\n")
  invisible(TRUE)
}

#' @rdname init
#' @export
install_textanalysis <- function(version = c("stable", "latest")){
  version <- match.arg(version)
  pkg <- "TextAnalysis"
  if(version == "latest") pkg <- "https://github.com/JuliaText/TextAnalysis.jl"
  julia_install_package(pkg)
}

#' @rdname init
#' @export
install_stringanalysis <- function(version = c("stable", "latest")){
  version <- match.arg(version)
  pkg <- "StringAnalysis"
  if(version == "latest") pkg <- "https://github.com/zgornel/StringAnalysis.jl"
  julia_install_package(pkg)
}