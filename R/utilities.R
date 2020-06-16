#' Making plots
#'
#' This function makes a plot of the selected Pareto front approximation.
#'
#' @param pfs A list of Pareto front approximations obtained by \code{opt} function.
#' @param run An integer denoting which run to depict in the plot.
#'
#' @return A plot depicting Pareto front approximation of the selected run.
#'
#' @export
make_plot <- function(pfs, run = 1) {
  if ((!run%%1 == 0) | (run < 1) | (run > length(pfs))) {
    stop("'run' not an integer or run not in [1,length(pfs)]")
  }
  pf <- pfs[[paste0("run.", run)]]
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggplot(pf, ggplot2::aes(x = -ce, y = pd)) +
      ggplot2::theme_bw() +
      ggplot2::xlab("f1: Collection efficency") +
      ggplot2::ylab("f2: Pressure drop") +
      ggplot2::geom_point()
  } else {
    plot(-pf$ce, pf$pd,
         xlab = "f1: Collection efficency",
         ylab = "f2: Pressure drop")
  }
}

#' Feasibility ratio
#'
#' This function calculates feasibility ratios.
#'
#' @param problem A list representing the problem configuration:
#' \describe{
#'		\item{\code{lower.bounds}}{Lower box constraints.}
#'		\item{\code{upper.bounds}}{Upper box constraints.}
#'		\item{\code{cons}}{Constraints considered in optimization.}
#' }
#' @param sample.size An integer denoting the sample size.
#'
#' @return A vector of feasibility ratios per constraints and overall feasibility ratio
#'
#' @export
feas_ratios <- function(problem, sample.size = 1e6) {

  if ("cons" %in% names(problem)) {
    if (is.null(problem$cons)) {
      cons <- NULL
    } else {
      cons <- problem$cons + 2
    }
  } else {
    cons <- 3:9
  }

  cons <- problem$cons
  pop <- t(replicate(sample.size, runif(6, problem$lower.bounds, problem$upper.bounds)))
  pop <- t(apply(pop, 1, fun_cyclone))
  frs <- apply(pop[, cons] <= 0, 2, sum) / nrow(pop)
  frs <- c(frs, sum(apply(pop[, cons] <= 0, 1, all)) / nrow(pop))
  names(frs) <- c(paste0("g", cons), "all")

  return(frs)
}
