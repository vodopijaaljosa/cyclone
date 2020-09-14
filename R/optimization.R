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
#' @return List of data frames storing the decision varaibles (x) and objective values (y) of all
#' nonodominated feasible solutions found during each run.
#'
#' @import emoa mco
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
  delta <- problem$delta
  intervals <- problem$intervals
  fluid <- problem$fluid
  cons.bound <- problem$cons.bound
  
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
      cons <- 3:9
    } else {
      cons <- problem$cons + 2
    }
  } else {
    cons <- 3:9
  }

  fun_objs <- function(x) fun_cyclone(x, fluid = fluid, delta = delta, intervals = intervals, cons.bound = cons.bound)[1:2]
  fun_cons <- function(x) -fun_cyclone(x, fluid = fluid, delta = delta, intervals = intervals, cons.bound = cons.bound)[cons]
  fun_both <- function(x) fun_cyclone(x, fluid = fluid, delta = delta, intervals = intervals, cons.bound = cons.bound)

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

    res <- find_pf(res, fun_both, cons)

    #prefix <- paste0("nsga2-mco-", problem$name)
    #res <- MOEAutils::save_run(res, prefix, fn = fun_both, objs = 1:2,
    #                           cons = cons, ref.point = c(1.1, 1.1))
    #res.x <- res$pf$x
    #res.x <- data.frame(res.x)
    #names(res.x) <- c("Da", "Dt", "H", "Ht", "He", "Be")
    #res.y <- res$pf$y
    #res.y <- data.frame(ce = 1 - res.y[, 1], pd = res.y[, 2])
    #res <- list(x = res.x, y = res.y)

  } else {
    res <- NULL
  }

  return(res)
}

### DEMO -----------------------------------------------------------------------
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

  fun_all <- function(x) {
    out <- fun_cyclone(x)[c(1, 2, cons)]
    out[3:length(out)] <- sapply(out[3:length(out)], function(z) max(z, 0))
    return(out)
  }

  res <- demo(fn         = fun_all,
              lower      = lower.bounds,
              upper      = upper.bounds,
              pop.size   = pop.size,
              no.iter    = no.iters,
              cross.prob = cross.prob,
              scal.fac   = mut.prob,
              no.cons    = length(cons))

  res <- find_pf(res, fun_cyclone, cons)

  return(res)
}

### SO -------------------------------------------------------------------------
opt_so <- function(problem,
                   method  = c("de", "jade", "cmaes"),
                   control = NULL,
                   no.runs = 1) {

  lower.bounds <- problem$lower.bounds
  upper.bounds <- problem$upper.bounds

  method <- match.arg(method)

  if (method %in% c("de", "jade")){
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

  pfs <- list()

  if (method == "de") {
    for (i in 1:no.runs) {
      if (!requireNamespace("DEoptim", quietly = TRUE)) stop("'DEoptim' package is required", call. = FALSE)
      pfs[[paste0("run.", i)]] <- run_de(problem = problem,
                                         control = control)
    }

  }

  return(pfs)
}

### DE -------------------------------------------------------------------------
run_de <- function(problem, control) {

  lower.bounds <- problem$lower.bounds
  upper.bounds <- problem$upper.bounds
  delta <- problem$delta
  intervals <- problem$intervals
  fluid <- problem$fluid
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
      cons <- 3:9
    } else if (!problem$cons) {
      cons <- NULL
    } else {
      cons <- problem$cons + 2
    }
  } else {
    cons <- 3:9
  }

  control.de <- DEoptim::DEoptim.control(NP = pop.size,
                                         itermax = no.iters,
                                         CR = cross.prob,
                                         F = mut.prob,
                                         c = 0.5,
                                         storepopfrom = 1,
                                         storepopfreq = 1,
                                         trace = FALSE)

  fun_all <- function(x) {
    out <- fun_cyclone(x, fluid = fluid, delta = delta, intervals = intervals)[c(1, cons)]
    out[2:length(out)] <- sapply(out[2:length(out)], function(z) max(z, 0))
    out <- sum(out)
    return(out)
  }

  fun_both <- function(x) fun_cyclone(x, fluid = fluid, delta = delta, intervals = intervals)

  res.de <- DEoptim::DEoptim(fn      = fun_all,
                             lower   = lower.bounds,
                             upper   = upper.bounds,
                             control = control.de)

  res <- res.de$member$storepop
  res <- find_opt_de(res, fun_both, cons)

  return(res)
}

### Helpers --------------------------------------------------------------------
find_pf <- function(res, fn, cons) {
  res  <- lapply(1:length(res), function(i) res[[i]]$par)
  res  <- do.call("rbind", res)
  res.y <- t(apply(res, 1, fn))
  if (is.null(cons)) {
    res.y <- res.y[, 1:2]
  } else if (length(cons) == 1) {
    res.y <- res.y[res.y[, cons] <= 0, 1:2]
  } else {
    keep <- apply(res.y[, cons] <= 0, 1, all)
    res  <- res[keep, ]
    res.y <- res.y[keep, 1:2]
  }
  keep <- emoa::nds_rank(t(as.matrix(res.y))) == 1
  res  <- res[keep, ]
  res.y <- res.y[keep, ]
  res.y <- data.frame(ce = res.y[, 1], pd = res.y[, 2])
  res <- data.frame(res)
  names(res) <- c("Da", "Dt", "H", "Ht", "He", "Be")
  return(list(x = res, y = res.y))
}

find_opt_de <- function(res, fn, cons) {
  res  <- do.call("rbind", res)
  res.y <- t(apply(res, 1, fn))

  if (is.null(cons)) {
    res.y <- res.y[, 1]
  } else if (length(cons) == 1) {
    res.y <- res.y[res.y[, cons] <= 0, 1]
  } else {
    keep <- apply(res.y[, cons] <= 0, 1, all)
    res  <- res[keep, ]
    res.y <- res.y[keep, 1]
  }
  keep <- which.min(res.y)
  res.y <- res.y[keep]
  res <- res[keep, ]
  return(list(x = res, y = res.y))
}


