#' Making plots
#'
#' This function makes a plot of the selected Pareto front approximation.
#'
#' @param res Output from \code{opt_mo}.
#' @param run An integer denoting which run to depict in the plot.
#' @param title A string denoting the title. Default is NULL.
#'
#' @return A plot depicting Pareto front approximation of the selected run.
#'
#' @export
make_plot <- function(res, run = 1, title = NULL) {
  # res.so = NULL, run.so = 1
  pf <- res[[paste0("run.", run)]]$y
  #so <- ifelse(is.null(res.so), NULL, 1 - res.so[[paste0("run.", run.so)]]$y)
  so <- max(1 - pf$ce) #todo: replace
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggplot(pf, ggplot2::aes(x = 1 - ce, y = pd)) +
      ggplot2::theme_bw() +
      ggplot2::xlab("f1: Collection efficency") +
      ggplot2::ylab("f2: Pressure drop") +
      ggplot2::geom_point() +
      ggplot2::labs(title = title) +
      ggplot2::xlim(0.9, 1) +
      ggplot2::ylim(0, 1) +
      ggplot2::geom_vline(xintercept = so, linetype = "dashed", color = "red", size=1)

  } else {
    plot(pf$ce, pf$pd,
         xlab  = "f1: Collection efficency",
         ylab  = "f2: Pressure drop",
         main  = title,
         xlim  = c(0.9, 1),
         ylim  = c(0, 1500))
  }
}

#' Feasibility ratios
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
#' @return A vector of feasibility ratios per constraints and the overall feasibility ratio.
#'
#' @export
feas_ratios <- function(problem, sample.size = 1e6) {

  if ("cons" %in% names(problem)) {
    if (is.null(problem$cons)) {
      cons <- 3:9
    } else if (!problem$cons) {
      cons <- NULL
    } else {
      cons <- problem$cons + 2
    }
  } else {
    cons <- 3:9
  }

  fun_both <- function(x) fun_cyclone(x, fluid = problem$fluid,
                                      intervals = problem$intervals,
                                      delta = problem$delta)

  pop <- t(replicate(sample.size, runif(6, problem$lower.bounds, problem$upper.bounds)))
  pop <- t(apply(pop, 1, fun_both))
  frs <- apply(pop[, cons] <= 0, 2, sum) / nrow(pop)
  frs <- c(frs, sum(apply(pop[, cons] <= 0, 1, all)) / nrow(pop))
  names(frs) <- c(paste0("g", cons - 2), "all")

  return(frs)
}

#' Creating problem instance
#'
#' This function creates problem instances based on default cyclone parameters
#'
#' @param cyclone Vector of default cyclone's geometrical parameters (Da, Dt, H, Ht, He, Be).
#' @param eps Float from [0, 1] denoting variance in gemoetrical parameters. Default is 0.1.
#'
#' @return A list of upper and lower bounds.
#'
#' @export
create_cmop <- function(prob, eps = 0.1, eskal.instance = 2) {
  default <- prob$default
  dc <- default * eps
  lower.bounds <- default - dc
  upper.bounds <- default + dc
  
  prob.eskal <- prob$eskal
  fluid <- prob$fluid
  cons <- prob$cons
  type <- prob$type
  
  if (!is.null(prob.eskal)) {
    delta <- eskal[[prob.eskal[eskal.instance]]]
    intervals <- eskal$intervals[1:(length(delta) + 1)]
    fluid$Rhop <- eskal$Rhop
  } else {
    delta <- NULL
    intervals <- NULL
  }
  
  if (is.null(cons)) cons <- 1:7
  
  if (!is.null(type)) {
    if (type == "low") {
      cons.bound <- list(
        E = 0.85, 
        deltaP = 1500, 
        geom.1 = 0, 
        geom.2 = 0.5
      )
    } else {
      cons.bound <- list(
        E = 0.9, 
        deltaP = 1500, 
        geom.1 = 1, 
        geom.2 = 0.44
      )
    }
  } else {
    cons.bound <- list(
      E = 0.9, 
      deltaP = 1500, 
      geom.1 = 1, 
      geom.2 = 0.5
    )
  }
  
  prob.out <- list(
    default = default, lower.bounds = lower.bounds, upper.bounds = upper.bounds, intervals = intervals, 
    delta = delta, fluid = fluid, cons = cons, cons.bound = cons.bound, name = prob$name
  )
  
  return(prob.out)
}

#' Computing parameter statistics
#'
#' This function computes the mean and sd of parameters given a set of solutions
#'
#' @param res Output from \code{opt_mo}.
#' @param run An integer denoting which run to use.
#' @param dec An integer denoting number of decimal points.
#'
#' @return Means and deviations of geometrical parameters.
#'
#' @export
param_stat <- function(res, run = 1, dec = 8) {
  res <- res[[paste0("run.", run)]]$x
  avg <- round(apply(res, 2, mean), dec)
  sd  <- round(apply(res, 2, sd), dec)
  return(list(mean = avg, sd = sd))
}
