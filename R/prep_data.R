#' Prepare data for mutli-state analysis
#'
#' Prepare data for a multi-state analysis either with survival::survfit or
#' mstate and/or flexsurvreg
#'
#' @param data data imported with import_data()
#' @param format either survival or mstate (default)
#'
#' @return a dataframe built with survival::tmerge or mstate::mprep; if format
#' is 'mstate', then the matrix of possible transitions (tmat) is returned as
#' an attribute
#'
#' @examples
#'
#' @importFrom survival tmerge
#' @importFrom mstate msprep
#'
#' @export

prep_data <- function(data, format = "mstate") {

  if (!(format %in% c("survival", "mstate"))) {
    stop("format must be either 'survival' or 'mstate'")
  }

  # ICU and rest times
  data$icu_time <- as.numeric(data$icu_in - data$hos_in)
  data$rest_time <- as.numeric(data$icu_out - data$hos_in)

  # People who leave directly hospital after ICU do not enter in rest state
  i <- with(data, !is.na(icu_out) & !is.na(hos_out) & icu_out == hos_out)
  data[i, "rest_time"] <- NA
  rm(i)

  # Recovery event
  data$recovery <- !is.na(data$hos_out) & !data$death

  # Follow-up time
  data$fu_time <- ifelse(!is.na(data$hos_out), data$hos_out,
                         attr(data, "censoring_date"))
  data$fu_time <- data$fu_time - as.numeric(data$hos_in)

  # Correct (adapt) null duration
  i <- with(data, !is.na(fu_time) & !is.na(icu_time) & fu_time - icu_time == 0)
  if (any(i)) data[i, "fu_time"] <- data[i, "fu_time"] + 0.2
  i <- with(data, !is.na(rest_time) & !is.na(icu_time) &
                    rest_time - icu_time == 0)
  if (any(i)) data[i, "rest_time"] <- data[i, "rest_time"] + 0.2
  data[!is.na(data$fu_time) & data$fu_time == 0, "fu_time"] <- 0.2
  data[!is.na(data$icu_time) & data$icu_time == 0, "icu_time"] <- 0.1
  rm(i)

  # ----------------------------------------------------------------- #
  # Prepare data for a multi-state analysis with the survival package #
  # ----------------------------------------------------------------- #

  if (format == "survival") {

    # Convert data from wide to long format
    msdata <- survival::tmerge(
      data1    = data[c("id", "sex", "age")],
      data2    = data,
      id       = id,
      death    = event(fu_time, death),
      recovery = event(fu_time, recovery),
      icu      = event(icu_time),
      rest     = event(rest_time)
    )
    msdata$icu  <- as.logical(msdata$icu)
    msdata$rest <- as.logical(msdata$rest)

    # Add event variable
    msdata$event <- factor(
      with(msdata, icu + 2 * rest + 4 * recovery + 8 * death),
      levels = c(0, 1, 2, 4, 8),
      labels = c("none", "icu", "rest", "recovery", "death")
    )

  }

  # ---------------------------------------------------------------------- #
  # Prepare data for a multi-state analysis with mstate and/or flexsurvreg #
  # ---------------------------------------------------------------------- #

  if (format == "mstate") {

    # Prepare data for mstate::msprep
    tmp_dat <- with(data, data.frame(
      id            = id,
      sex           = sex,
      age           = age,
      icu           = !is.na(icu_time),
      rest          = !is.na(rest_time),
      death         = death,
      recovery      = recovery,
      icu_time      = ifelse(is.na(icu_time), fu_time, icu_time),
      rest_time     =  ifelse(is.na(rest_time), fu_time, rest_time),
      death_time    = fu_time,
      recovery_time = fu_time
    ))

    # States
    states <- c("hos", "icu", "rest", "death", "recovery")

    # Matrix describing allowed transitions
    tmat <- matrix(NA, 5, 5)
    dimnames(tmat) <- list(from = states, to  = states)
    tmat["hos", c("icu", "death", "recovery")] <- 1:3
    tmat["icu", c("rest", "death", "recovery")] <- 4:6
    tmat["rest", c("death", "recovery")] <- 7:8

    # Multistate data for mstate and flexsurv
    msdata <- mstate::msprep(
      time = c(NA, paste0(states[-1], "_time")),
      status = c(NA, states[-1]),
      data = tmp_dat,
      trans = tmat,
      id = "id", 
      keep = c("sex", "age")
    )
    msdata$trans <- factor(msdata$trans)

    # Add tmat as an attribute
    attr(msdata, "tmat") <- tmat

  }

  # Return prepared data
  return(msdata)

}
