#' Random sample for exponential growth parameter
#'
#' Random sample for exponential growth parameter (constrained >1)
#'
#' @param n number of draws
#' @param megp median of growth parameter (typically 1.25)
#' @param vegp variability
#'
#' @return a vector sampled from the exponential growth parameter distribution
#'
#' @examples
#'
#' @importFrom stats rnorm
#'
#' @export

regp <- function(n, megp, vegp) {
  1 + exp(rnorm(n, log(megp - 1), sqrt(vegp)))
}
