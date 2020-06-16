### MOEA --------------------------------------------------------------------------------
#' Optimization
#'
#' This function solves the cyclone optimization problem
#'
#' @param problem A list representing the problem configuration:
#' \describe{
#'		\item{\code{lower.bounds}}{Lower box constraints.}
#'		\item{\code{upper.bounds}}{Upper box constraints.}
#'		\item{\code{cons}}{Constraints considered in optimization.}
#' }
#' @param method A string indicating the used optimization algorithm.
#' It can be NSGA-II ("nsga2"), DEMO ("demo") or MOEA/D ("moead"). Default is DEMO.
#' @param control A list of control parameters:
#' #' \describe{
#'		\item{\code{pop.size}}{An integer denoting the number of solutions
#'		used by the optimization algorithm. Default is 100.}
#'		\item{\code{no.iters}}{An integer denoting the number of iterations
#'		(generations) used by the optimization algorithm. Default is 100.}
#'		\item{\code{cross.prob}}{A float from [0,1] denoting the crossover
#'		probability. Default is 0.9.}
#'		\item{\code{mut.prob}}{A float from [0,1] denoting the mutation probability.
#'		For DEMO this is the scaling factor. Default is 0.1 for NSGA-II and MOEA/D,
#'		while 0.7 for DEMO.}
#' }
#' Default values are used for unspecified parameters.
#' @param no.runs An integer denoting the number of runs. Default is 1.
#'
#' @return List of data frames storing the objective values of all
#' nonodominated feasible solutions found during each run.
#'
#' @export
opt_mo <- function(problem,
                   method  = c("nsga2", "demo"),
                   control = NULL,
                   no.runs = 1) {

  method <- match.arg(method)

  if (method == "demo"){
    if (is.null(control)) {
      control <- list(mut.prob = 0.7)
    } else {
      control$mut.prob <- ifelse("mut.prob" %in% names(control), control$mut.prob, 0.7)
    }
  }

  control <- list(pop.size   = ifelse("pop.size" %in% names(control), control$pop.size, 100),
                  no.iters   = ifelse("no.iters" %in% names(control), control$no.iters, 100),
                  cross.prob = ifelse("cross.prob" %in% names(control), control$cross.prob, 0.9),
                  mut.prob   = ifelse("mut.prob" %in% names(control), control$mut.prob, 0.1))

  if ((control$mut.prob < 0) | (control$mut.prob > 1)) {
    stop("'mut.prob' not in [0,1]")
  }

  if ((control$cross.prob < 0) | (control$cross.prob > 1)) {
    stop("'cross.prob' not in [0,1]")
  }

  pfs <- list()

  if (method == "nsga2") {
    for (i in 1:no.runs) {
      if (!requireNamespace("mco", quietly = TRUE)) stop("'mco' package is required", call. = FALSE)
      pfs[[paste0("run.", i)]] <- run_nsga2(problem = problem,
                                            control = control)
    }

  } else {
    for (i in 1:no.runs) {
      pfs[[paste0("run.", i)]] <- run_demo(problem = problem,
                                           control = control)
    }
  }

  return(pfs)
}

### NSGA-II -----------------------------------------------------------------------------
run_nsga2 <- function(problem, control) {

  lower.bounds <- problem$lower.bounds
  upper.bounds <- problem$upper.bounds
  pop.size   <- control$pop.size
  no.iters   <- control$no.iters
  cross.prob <- control$cross.prob
  mut.prob   <- control$mut.prob

  if (requireNamespace("memoise", quietly = TRUE)){
    tryCatch({
      fun_cyclone <- memoise::memoise(fun_cyclone)
    }, error = function(err) {
    })
  }

  if ("cons" %in% names(problem)) {
    if (is.null(problem$cons)) {
      cons <- NULL
    } else {
      cons <- problem$cons + 2
    }
  } else {
    cons <- 3:9
  }

  fun_objs <- function(x) fun_cyclone(x)[1:2]
  fun_cons <- function(x) -fun_cyclone(x)[cons]

  if (requireNamespace("mco", quietly = TRUE)){
    res <- mco::nsga2(fn           = fun_objs,
                      idim         = length(lower.bounds),
                      odim         = 2,
                      lower.bounds = lower.bounds,
                      upper.bounds = upper.bounds,
                      constraints  = fun_cons,
                      cdim         = length(cons),
                      popsize      = pop.size,
                      cprob        = cross.prob,
                      mprob        = mut.prob,
                      generations  = 1:no.iters)
    pf <- find_pf(res, fun_cyclone, cons)
  } else {
    pf <- NULL
  }

  return(pf)
}

### DEMO --------------------------------------------------------------------------------
run_demo <- function(problem, control) {

  lower.bounds <- problem$lower.bounds
  upper.bounds <- problem$upper.bounds
  pop.size   <- control$pop.size
  no.iters   <- control$no.iters
  cross.prob <- control$cross.prob
  mut.prob   <- control$mut.prob

  if (requireNamespace("memoise", quietly = TRUE)){
    tryCatch({
      fun_cyclone <- memoise::memoise(fun_cyclone)
    }, error = function(err) {
    })
  }

  fun_all <- function(x) {
    out <- fun_cyclone(x)
    out[3] <- max(out[3], 0)
    out[4] <- max(out[4], 0)
    out[5] <- max(out[5], 0)
    out[6] <- max(out[6], 0)
    out[7] <- max(out[7], 0)
    out[8] <- max(out[8], 0)
    out[9] <- max(out[9], 0)
    return(out)
  }

  res <- demo(fn         = fun_all,
              lower      = lower.bounds,
              upper      = upper.bounds,
              pop.size   = pop.size,
              no.iter    = no.iters,
              cross.prob = cross.prob,
              scal.fac   = mut.prob,
              no.cons    = 3)

  pf <- find_pf(res, fun_cyclone)

  return(pf)
}

### SO ----------------------------------------------------------------------------------
opt_so <- function(problem, obj, control = NULL) {
  lower.bounds <- problem$lower.bounds
  upper.bounds <- problem$upper.bounds

  control <- list(pop.size   = ifelse("pop.size" %in% names(control), control$pop.size, 100),
                  no.iters   = ifelse("no.iters" %in% names(control), control$no.iters, 200),
                  cross.prob = ifelse("cross.prob" %in% names(control), control$cross.prob, 0.9),
                  mut.prob   = ifelse("mut.prob" %in% names(control), control$mut.prob, 0.1))

  control.de <- DEoptim::DEoptim.control(NP      = control$pop.size,
                                         itermax = control$no.iters,
                                         CR      = control$cross.prob,
                                         F       = control$mut.prob,
                                         c       = 0.5)

  if (requireNamespace("memoise", quietly = TRUE)){
    tryCatch({
      fun_cyclone <- memoise::memoise(fun_cyclone)
    }, error = function(err) {
    })
  }

  fn <- function(x) fun_cyclone(x)[obj]

  res <- DEoptim::DEoptim(fn      = fn,
                          lower   = lower.bounds,
                          upper   = upper.bounds,
                          control = control.de)
}

### Helpers -----------------------------------------------------------------------------
find_pf <- function(res, fn, cons) {
  res <- lapply(1:length(res), function(i) res[[i]]$par)
  res <- do.call("rbind", res)
  res <- t(apply(res, 1, fn))
  if (is.null(cons)) {
    res <- res[, 1:2]
  } else if (length(cons) == 1) {
    res <- res[res[, cons] <= 0, 1:2]
  } else {
    res <- res[apply(res[, cons] <= 0, 1, all), 1:2]
  }
  res <- t(emoa::nondominated_points(t(as.matrix(res))))
  res <- data.frame(ce = res[, 1], pd = res[, 2])
  return(res)
}
