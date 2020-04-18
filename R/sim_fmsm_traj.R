#' Generate trajectories for given covariates
#'
#' Generate trajectories from a semi-Markov model built with
#' flexsurv::flexsurvreg. The trajectories are simulate with
#' flexsurv::sim.fmsm. The latter return the times of the transitions.  This
#' function determines the states M for each individual for each time given in
#' an input vector.
#'
#' @param ... arguments passed to sim.fmsm (exept t which is calculated from
#' tvec)
#' @param tvec a vector of times for which the states have to be determined
#'
#' @return a matrix of states, each row coresponds to an individual and each
#' column to a time
#'
#' @examples
#'
#' @importFrom flexsurv sim.fmsm
#'
#' @export

sim_fmsm_traj <- function(..., tvec) {

  sim <- flexsurv::sim.fmsm(..., t = max(tvec))
  sapply(tvec, function(s) {
    j <- apply(sim$t <= s, 1, sum)
    sim$st[cbind(seq_along(j), j)]
  })

}
