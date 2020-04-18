#' Generate daily data
#'
#' Generate daily data from REDCap data in wide or long format
#'
#' @param data data imported with import_data()
#' @param format either wide (default) or long
#'
#' @return a dataframe
#' variables in wide format:
#' \itemize{
#'   \item date     = date
#'   \item cumhos   = cumulative number of hospitalized patients
#'   \item hos      = current number of hospitalized patients
#'   \item icu      = current number of patients treated in intensive care
#'   \item rest     = current number of patients hospitalized after
#'                    intensive care
#'   \item death    = cumulative number of deaths
#'   \item recovery = cumulative number of recoveries
#' }
#' variables in long format:
#' \itemize{
#'   \item date     = date
#'   \item state    = hos, icu, rest, death or recovery
#'   \item n        = count
#' }
#'
#' @examples
#'
#' @export

daily_data <- function(data, format = "wide") {

  days <- seq(min(data$hos_in), attr(data, "censoring_date"), 1)

  daily <- do.call(rbind, lapply(days, function(u) {
    hos_in <- data$hos_in
    icu_in <- data$icu_in
    hos_out <- data$hos_out
    icu_out <- data$icu_out
    dead <- data$death
    cumhos <- sum(hos_in <= u)
    death <- sum(!is.na(hos_out) & u >= hos_out & dead)
    recovery <- sum(!is.na(hos_out) & u >= hos_out & !dead)
    rest <- sum(!is.na(icu_out) & u >= icu_out &
                  (is.na(hos_out) | u < hos_out))
    icu <- sum(!is.na(icu_in) & u >= icu_in & (is.na(icu_out) | u < icu_out))
    hos <- cumhos - icu - rest - recovery - death
    c(cumhos = cumhos, hos = hos, icu = icu, rest = rest, death = death,
      recovery = recovery)
  }))

  daily <- cbind(date = days, as.data.frame(daily))

  if (format == "long") {
    daily <- daily[names(daily) != "cumhos"]
    states <- c("hos", "icu", "rest", "death", "recovery")
    daily <- reshape(daily, varying = states, v.names = "n", timevar = "state",
                     idvar = "date", times = states, direction = "long")
    rownames(daily) <- NULL
  }

  return(daily)

}
