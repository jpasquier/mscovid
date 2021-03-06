#' Generate trajectories from flexsurv::sim.fmsm
#'
#' Generate trajectories from the simulations generated by flexsurv::sim.fmsm
#' for a semi-Markov model built with flexsurv::flexsurvreg. The function
#' flexsurv::sim.fmsm returns the times of the transitions.  This
#' function determines each time given in the vector tvec.
#'
#' @param sim output of flexsurv::sim.fmsm
#' @param tvec a vector of times for which the states have to be determined
#'
#' @return a matrix of states, each row coresponds to an individual and each
#' column to a time
#'
#' @examples
#'
#' @export

sim_to_traj <- function(sim, tvec) {

  sapply(tvec, function(s) {
    j <- apply(sim$t <= s, 1, sum)
    as.integer(sim$st[cbind(seq_along(j), j)])
  })

}
