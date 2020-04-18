#' Simulate state occupancy
#'
#' Simulate state occupancy of new patients for a given number of days.
#'
#' @param fit multi-state model (or list of models, one per possible
#' transition) estimated with flexsurv::flexsurvreg
#' @param trans matrix describing the possible transitions
#' @param newdata dataframe containing the covariates of the new patients (one
#' row per new patient)
#' @param nday number of days to simulate
#'
#' @return a matrix containing the number of patients per state (rows) and per
#' day (cols)
#'
#' @examples
#'
#' @importFrom stats aggregate
#'
#' @export

sim_state_occupancy <- function(fit, trans, newdata, nday) {

  # Aggragate individuals by covariables
  newdata <- aggregate(count ~ sex + age, cbind(count = 1, newdata), sum)

  # Compute trajectories per covariables profiles
  traj <- do.call(rbind, lapply(1:nrow(newdata), function(i) {
    sim_fmsm_traj(fit, trans = trans, newdata = newdata[i, ], 
                  M = newdata[i, "count"], tvec = (0:(nday - 1)) + 0.5)
  }))

  # Matrix containing the number of patients per state (rows)
  # and per day (cols)
  nstate <- nrow(tmat)
  do.call(rbind, lapply(1:nstate, function(i) apply(traj == i, 2, sum)))

}
