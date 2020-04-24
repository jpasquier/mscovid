#' Predict state occupancy
#'
#' Predict state occupancy for 1.25 multi-state model
#'
#' @param fit multi-state model (or list of models, one per possible
#' transition) estimated with flexsurv::flexsurvreg
#' @param trans matrix describing the possible transitions
#' @param nday number of days to predict
#' @param nsim number of simulations
#' @param pars table of parameter
#' @param data data imported with import_data()
#' @param start_date date to start simulations
#' @param seed seed of the random number generator for reproducible analyses
#' @param ncpu number of cpus to use for parallel computations
#'
#' @return a list of simultations (each element of the list correspond to a
#' different state); the data are also returned as an attribute (data)
#'
#' @examples
#'
#' @importFrom parallel detectCores mclapply
#' @importFrom flexsurv sim.fmsm
#' @importFrom utils flush.console
#'
#' @export

pred_state_occupancy <- function(fit, trans, nday, nsim, pars, data,
                                 start_date = "2020-02-25", seed = 666,
                                 ncpu = 1) {

  #
  t0 <- proc.time()

  # Init gen rand nbr
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)

  # Cumulative data
  daily <- subset(daily_data(data, "wide"),
                  date >= as.Date(start_date), c("date", "cumhos"))
  names(daily)[2] <- "nhos"

  # Check consistency of dates
  if(min(pars$date) > min(daily$date)){
    stop(paste("First date defining parameters must be anterior or",
               "equal to first date in the data!"))
  }

  # today i.e. last date entered in data
  today <- daily$date[nrow(daily)]

  # vector of days for observed data and predictions
  days <- c(daily$date, today + (1:nday))

  # index of today
  j <- which(days == today)

  # Get parameters for each day in "days"
  ind <- sapply(days, function(d) max(which(pars$date <= d)))
  megp <- pars$megp[ind]
  vegp <- pars$vegp[ind]

  # Fill-in observed cumulative count of hospitalized patients
  nhos <- matrix(nrow = nsim, ncol = j + nday)
  nhos[, 1:j] <- t(daily$nhos)[rep(1, nsim), ]

  # Predict future cumulative counts of hospitalized patients using
  # exponential growth parameter
  for(k in (j + 1):(j + nday)) {
    nhos[, k] <- round(nhos[, k - 1] * regp(nsim, megp[k], vegp[k]))
  }

  # Calculate daily new hospitalizations (i.e. incident counts)
  ninc <- matrix(nrow = nsim, ncol = j + nday)
  ninc[, 1] <- nhos[, 1]
  for(k in 1:(j + nday - 1)) {
    ninc[, k + 1] <- nhos[, k + 1] - nhos[, k]
  }
  if(sum(ninc < 0) > 0) {
    stop("Some incident counts are negative!")
  }

  # number of states
  nstate <- nrow(tmat)

  # New individual profiles, per day and simulation, in long format
  newdata <- do.call(rbind, lapply(1:nrow(ninc), function(i) {
    do.call(rbind, lapply(1:ncol(ninc), function(j) {
      cbind(sim = rep(i, ninc[i, j]), day = rep(j, ninc[i, j]))
    }))
  }))
  newdata <- cbind(
    newdata,
    data[sample.int(nrow(data), nrow(newdata), replace = TRUE),
         c("sex", "age")]
  )

  # Unique profiles
  profiles <- unique(newdata[c("sex", "age")])

  # Simulations of the trajectory of each individual
  sim <- do.call(rbind, mclapply(1:nrow(profiles), function(i) {
    # Select individuals per profile
    nd <- newdata[newdata$sex == profiles$sex[i] & 
                    newdata$age == profiles$age[i], ]
    # Number of days to simulate
    nd$nday <- ncol(ninc) - nd$day + 1
    # Simulations with flexsurv::sim.fmsm
    sim <- sim.fmsm(fit, trans = trans, t = nday, newdata = profiles[i, ],
                    M = nrow(nd))
    # Convert results to trajectories
    tvec <- (0:(max(nd$nday) - 1)) + 0.5
    traj <- sim_to_traj(sim = sim, tvec = tvec)
    # Adjust for day of entry
    traj <- do.call(rbind, lapply(1:nrow(nd), function(i) {
      c(rep(0L, nd$day[i] - 1), traj[i, 1:(ncol(ninc) - nd$day[i] + 1)])
    }))
    # return trajectories
    colnames(traj) <- paste0("state_day", 1:ncol(ninc))
    cbind(nd, traj)
  }, mc.cores = ncpu))

  # Number of individual per state (list), simulation (row) and day (col)
  Z <- sim[, grep("^state_day[0-9]+", names(sim))]
  S <- mclapply(mc.cores = ncpu, 1:nstate, function(k) {
    Z1 <- Z == k
    n <- do.call(rbind, lapply(1:nsim, function(i) {
      apply(Z1[sim$sim == i, ], 2, sum)
    }))
    colnames(n) <- as.character(days)
    return(n)
  })
  names(S) <- attr(tmat, "dimnames")[[1]]

  # Add data as an attribute
  attr(S, "data") <- data

  #
  t1 <- proc.time()
  cat("Calculations completed in", ceiling((t1 - t0)[3]), "seconds", "\n")
  flush.console()

  # return results
  return(S)

}

