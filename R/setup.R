#' Initialise Session
#' 
#' Initialise a session, installs TextAnalysis Juia dependency if needed.
#' 
#' @param ... Arguments passed to \link[JuliaCall]{julia_setup}.
#' 
#' @section Packages:
#' Packages installed by \code{init_textanalysis} are:
#' \itemize{
#'   \item{\code{TextAnalysis}}
#'   \item{\code{Languages}}
#' }
#' Packages installed by \code{init_stats} are:
#' \itemize{
#'   \item{\code{MultivariateStats}}
#'   \item{\code{Clustering}}
#' }
#' 
#' @import JuliaCall
#' @import assertthat
#' 
#' @name init
#' @export
init_textanalysis <- function(...){
  julia <- JuliaCall::julia_setup(...)
  JuliaCall::julia_install_package_if_needed("TextAnalysis")
  JuliaCall::julia_install_package_if_needed("Languages")
  JuliaCall::julia_library("TextAnalysis")
}

#' @rdname init
#' @export
init_stats <- function(){
  JuliaCall::julia_install_package_if_needed("MultivariateStats")
  JuliaCall::julia_install_package_if_needed("Clustering")
  JuliaCall::julia_library("MultivariateStats")
  JuliaCall::julia_library("Clustering")
}

#' @rdname init
#' @export
setup_textanalysis <- init_textanalysis