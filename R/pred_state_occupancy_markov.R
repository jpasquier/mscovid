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
#' @importFrom flexsurv pmatrix.fs
#' @importFrom utils flush.console
#' @import data.table
#'
#' @export

pred_state_occupancy_markov <- function(fit, trans, nday, nsim, pars, data,
                                        start_date = "2020-02-25", seed = 666,
                                        ncpu = 1) {

  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)

  #
  t0 <- proc.time()

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

  # Profiles weights (for sampling)
  profiles <- aggregate(id ~ sex + age_class, data, length)
  profiles <- profiles[order(profiles$sex, profiles$age_class), ]
  names(profiles)[names(profiles) == "id"] <- "weight"
  profiles <- cbind(pid = 1:nrow(profiles), profiles)

  # New individual profiles, per day and simulation, in long format
  newdata <- do.call(rbind, lapply(1:nrow(ninc), function(i) {
    do.call(rbind, lapply(1:ncol(ninc), function(j) {
      cbind(sim = rep(i, ninc[i, j]), day = rep(j, ninc[i, j]))
    }))
  }))
  newdata <- cbind(newdata, profile = sample(
    profiles$pid, nrow(newdata), replace = TRUE, prob = profiles$weight))
  newdata <- as.data.frame(newdata)
  setDT(newdata)
  newdata <- newdata[, .N, by = names(newdata)]

  # Transition probabilities
  P <- do.call(rbind, lapply(profiles$pid, function(i) {
    p <- pmatrix.fs(fits_markov, trans = tmat, t = c(0.5, 1:(ncol(ninc) - 1)),
                    newdata = profiles[profiles$pid == i, ])
    p <- do.call(rbind, lapply(p, function(z) z[1, ]))
    colnames(p) <- colnames(trans)
    cbind(profile = i, day_inc = 0:(ncol(ninc) - 1), p)
  }))
  P <- as.data.frame(P)
  setDT(P)

  # Number of people in each state
  sim <- rbindlist(lapply(sort(unique(newdata$day)), function(i) {
    newdata[day == i][P[day_inc <= ncol(ninc) - i], on = "profile",  allow.cartesian = TRUE]
  }))
  sim[, day := day + day_inc][, day_inc := NULL]
  n <- sim[, 
           .(hos = sum(N * hos),
             icu = sum(N * icu),
             rest = sum(N * rest),
             death = sum(N * death),
             recovery = sum(N * recovery)),
            by = .(sim, day)]

  # Results of the simulation per state
  S <- lapply(colnames(tmat), function(s) {
    n <- as.matrix(
      dcast(n, sim ~ day, value.var = s)[order(sim)][, sim := NULL]
    )
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

