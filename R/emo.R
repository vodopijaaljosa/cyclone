# CDP selection
cdpSelection <- function(x, n, no.cons = 0) {
  x <- t(x)
  N <- ncol(x)
  ranks <- doCdpdSorting(x, no.cons = no.cons)$ranks
  sel <- rep(FALSE, N)
  cr <- 0
  while (sum(sel) < n) {
    cr <- cr + 1
    sel[ranks == cr] <- TRUE
  }
  if (sum(sel) != n) {
    nelim <- sum(sel) - n
    dist <- emoa::crowding_distance(x[, ranks == cr])
    cdr <- rank(dist, ties.method = "random")
    s <- which(ranks == cr)[cdr <= nelim]
    sel[s] <- FALSE
  }
  return(sel)
}

# Old pure R implementation from package erc
doCdpdSorting = function(x, no.cons = 0) {
  # initialize domination front wrapper
  fronts = list()
  fronts[[1L]] = list()

  n = ncol(x)
  dom.counter = integer(n)
  ranks = integer(n)
  dom.els = vector(mode = "list", length = n)

  # compute domination numbers and Pareto front
  for (i in seq.int(n)) {
    for (j in seq.int(n)) {
      if (dominates(x[, i], x[, j], no.cons = no.cons)) {
        dom.els[[i]] = c(dom.els[[i]], j)
      } else if (isDominated(x[, i], x[, j], no.cons = no.cons)) {
        dom.counter[i] = dom.counter[i] + 1L
      }
    }
    # in this case point x_i belongs to the Pareto front (domination layer 1)
    if (dom.counter[i] == 0L) {
      ranks[i] = 1L
      fronts[[1L]] = c(fronts[[1L]], i)
    }
  }

  # make a copy of the domination number since we are going to modify these
  # in the next lines, but also want to return them
  dom.counter2 = dom.counter

  # now compute the remaining domination fronts
  k = 1L
  while (length(fronts[[k]]) > 0L) {
    front2 = list()
    for (i in fronts[[k]]) {
      for (j in dom.els[[i]]) {
        dom.counter[j] = dom.counter[j] - 1L
        if (dom.counter[j] == 0L) {
          ranks[j] = k + 1L
          front2 = c(front2, j)
        }
      }
    }
    k = k + 1L
    fronts[[k]] = front2
  }

  return(
    list(
      ranks = ranks,
      dom.counter = dom.counter2 # assign the unmodified version
    )
  )
}

# dominates
dominates = function(x, y, no.cons = 0) {
  stopifnot(length(x) == length(y))
  if (no.cons == 0){
    return(all(x <= y) && any(x < y))
  } else {
    x.obj <- head(x, length(x) - no.cons)
    y.obj <- head(y, length(y) - no.cons)
    x.phi <- sum(tail(x, no.cons))
    y.phi <- sum(tail(y, no.cons))
    if (x.phi == 0 & y.phi == 0) {
      return(all(x.obj <= y.obj) && any(x.obj < y.obj))
    } else {
      return(x.phi < y.phi)
    }
  }
}

# is dominated
isDominated = function(x, y, no.cons = 0) {
  return(dominates(y, x, no.cons = no.cons))
}
