#' Convert a string or a POSIXt in a Date
#'
#' Convert a string or a POSIXt in a Date
#'
#' @param x a date in string or POSIXt format
#' @param format date format (for strings only)
#'
#' @return a date in the Date format
#'
#' @examples
#'
#' @export

conv_date <- function(x, format = "%d.%m.%Y") {
  if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
    x <- as.Date(x)
  } else if (any(class(x) %in% "character")) {
    x <- as.Date(strptime(as.character(x), format = format))
  } else if (!any(class(x) %in% "Date")) {
    stop("Wrong date format")
  }
  return(x)
}
