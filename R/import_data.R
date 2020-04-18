#' Import REDCap data
#'
#' Import a formatted Excel REDCap datafile containing the following variables:
#' \itemize{
#'   \item no_unique            = anonymized identifier
#'   \item sex                  = patient's gender (M/F)
#'   \item age                  = patient's age (years)
#'   \item arrivee_hopital      = date of hospitalization
#'   \item debut_soins_intensif = date of admission in ICU
#'   \item fin_soins_intensifs  = intensive care discharge date
#'   \item sortie_hopital       = discharge date from hospital
#'   \item deces                = death indicator (0/1)
#' }
#' The four date variable must be in date format in Excel. A censoring date
#' could be defined. If such a date is not given, it is guessed based on the
#' data.
#'
#' @param path path to data file
#' @param censoring_date date used for censoring (optional, format: \%Y-\%m-\%d)
#' @param ... other parameter passed to readxl::read_xlsx
#'
#' @return a dataframe containing the following variables
#' \itemize{
#'   \item id      = anonymized identifier
#'   \item sex     = patient's gender (factor M/F)
#'   \item age     = patient's age (years)
#'   \item hos_in  = date of hospitalization
#'   \item icu_in  = date of admission in ICU
#'   \item icu_out = intensive care discharge date
#'   \item hos_out = discharge date from hospital
#'   \item death   = death indicator (logical true/false)
#' }
#' The censoring date is returned as an attributes (censoring_date)
#'
#' @examples
#'
#' @importFrom readxl read_xlsx
#'
#' @export

import_data <- function(path, ..., censoring_date = NULL) {

  # Data
  data <- readxl::read_xlsx(path, ...)
  data <- as.data.frame(data, stringAsFactors = FALSE)

  # Recoding
  data$id      <- data$no_unique
  data$sex     <- factor(data$sex)
  data$hos_in  <- as.Date(data$arrivee_hopital)
  data$icu_in  <- as.Date(data$debut_soins_intensifs)
  data$icu_out <- as.Date(data$fin_soins_intensifs)
  data$hos_out <- as.Date(data$sortie_hopital)
  data$death   <- data$deces == 1

  # Censoring date
  if (!is.null(censoring_date)) {
    censoring_date <- as.Date(censoring_date)
  } else {
    censoring_date <-
      with(data, max(c(hos_in, icu_in, icu_out, hos_out), na.rm = TRUE))
  }

  # Check data
  if (any(is.na(data$hos_in))) {
    stop("Missing hospitalization dates")
  }
  if (any(is.na(data$icu_in) & !is.na(data$icu_out))) {
    stop("Missing ICU admission dates")
  }
  if (with(data, any(!is.na(icu_in) & is.na(icu_out) & !is.na(hos_out)))) {
    stop("Missing ICU out dates")
  }
  if (with(data, any(!is.na(hos_out) & hos_out < hos_in))) {
    stop("hos_out < hos_in")
  }
  if (with(data, any(!is.na(icu_in) & !is.na(icu_out) & icu_out < icu_in))) {
    stop("icu_out < icu_in")
  }
  if (with(data, any(!is.na(icu_out) & !is.na(hos_out) & hos_out < icu_out))) {
    stop("icu_out < icu_in")
  }
  if (any(is.na(data$deces))) {
    stop("Missing death indicators")
  }
  if (any(data$deces == 1 & is.na(data$hos_out))) {
    stop("Missing dates of death")
  }
  if (with(data, max(c(hos_in, icu_in, icu_out, hos_out), na.rm = TRUE)) >
        censoring_date) {
    stop("Current date is incorrectly set")
  }

  # Select variables
  X <- c("id", "sex", "age", "hos_in", "icu_in", "icu_out", "hos_out", "death")
  data <- data[X]

  # Add censoring date attribute
  attr(data, "censoring_date") <- censoring_date

  # return data
  return(data)

}

