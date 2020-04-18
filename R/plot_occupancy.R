#' Plot state occupancy
#'
#' Plot state occupancy for 1.25 multi-state model
#'
#' @param pred an object returned by pred_state_occupancy
#' @param state a vector containing one or more of the following values: 'hos',
#' 'icu', 'rest', 'death', 'recovery'; use 'all' to select all the states
#' @param pi a logical value (TRUE/FALSE) indicating if prediction intervall
#' must be drawn
#' @param pi_length length of predicion intervall (ignored if pi = FALSE)
#' @param ttl plot title
#' @param sttl plot subtitle
#'
#' @return a plot of state occupancy
#'
#' @examples
#'
#' @import ggplot2
#'
#' @export

plot_occupancy <- function(pred, state = "all", pi = FALSE, pi_length = 0.9,
                           ttl = "", sttl = "") {

  states <- c("hos", "icu", "rest", "death", "recovery")

  if ("all" %in% state) {
    state <- states
  } else {
    state <- unique(state[state %in% states])
  }
  if (length(state) == 0) {
    stop("The variable state must contain at least one valid value")
  }

  p <- c(0.5, 0:1 + c(1, -1) * (1 - pi_length) / 2)

  counts <- do.call(rbind, lapply(state, function(s) {
    n <- as.data.frame(t(apply(pred[[s]], 2, quantile, prob = p)))
    names(n) <- c("n", "lwr", "upr")
    n <- cbind(date = as.Date(rownames(n)), state = s, n)
  }))

  counts0 <- daily_data(attr(pred, "data"), "long")
  counts0 <- counts0[counts0$state %in% state, ]
  counts0 <- counts0[counts0$date >= min(counts$date), ]

  fig <- ggplot(counts, aes(x = date, y = n, colour = state)) +
    geom_line() +
    geom_point(data = counts0, show.legend = FALSE) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = ttl, subtitle = sttl, x = "Date", y = "Counts") +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "top")

  if (pi) {
    fig <- fig +
      geom_ribbon(aes(ymin = lwr, ymax = upr, colour = NULL, fill = state),
                  alpha = 0.2, show.legend = FALSE) +
      scale_fill_brewer(palette = "Dark2")
  }

  return(fig)

}
