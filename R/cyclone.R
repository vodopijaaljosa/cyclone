#' Objective function - Cyclone Simulation: Barth/Muschelknautz
#'
#' Calculate cyclone collection efficiency. A simple, physics-based
#' optimization problem (potentially bi-objective). See the references [1,2].
#'
#' @param cyclone vector of cyclone's geometrical parameters (Da, Dt, H, Ht, He, Be)
#' @param fluid list of default fluid parameters (Mu, Ve, lambdag, Rhop, Rhof, Croh)
#' @param intervals vector specifying the particle size interval bounds
#' @param delta vector, amount of dust (percentage value) in each interval
#' @param cons.bounds vector of objective bounds for (CEmin, PDmax)
#'
#' @return a vector (-CE, PD, geometrical constraints, CE constraint, PD constraint)
#'
#' @references
#' [1] Zaefferer, M.; Breiderhoff, B.; Naujoks, B.; Friese, M.; Stork, J.; Fischbach, A.; Flasch, O.; Bartz-Beielstein, T. Tuning Multi-objective Optimization Algorithms for Cyclone Dust Separators Proceedings of the 2014 Conference on Genetic and Evolutionary Computation, ACM, 2014, 1223-1230 \cr\cr
#' [2] Breiderhoff, B.; Bartz-Beielstein, T.; Naujoks, B.; Zaefferer, M.; Fischbach, A.; Flasch, O.; Friese, M.; Mersmann, O.; Stork, J.; Simulation and Optimization of Cyclone Dust Separators Proceedings 23. Workshop Computational Intelligence, 2013, 177-196
#'
#' @export
fun_cyclone <- function(cyclone,
                        fluid = NULL,
                        intervals = NULL,
                        delta = NULL,
                        #ratio.cut = NULL,
                        cons.bound = NULL){

  Da <- cyclone[1]
  Dt <- cyclone[2]
  H  <- cyclone[3]
  Ht <- cyclone[4]
  He <- cyclone[5]
  Be <- cyclone[6]
  
  if (is.null(intervals)) intervals <- c(0, 2, 4, 6, 8, 10, 15, 20, 30) * 1e-6
  if (is.null(delta)) delta <- c(0.0, 0.02, 0.03, 0.05, 0.1, 0.3, 0.3, 0.2)
  #if (is.null(ratio.cut)) ratio.cut <- 1
  
  fluid <- list(
    Mu = ifelse("Mu" %in% names(fluid), fluid$Mu, 1.85e-5),
    Vp = ifelse("Vp" %in% names(fluid), fluid$Vp, 1.3889),
    lambdag = ifelse("lambdag" %in% names(fluid), fluid$lambdag, 5e-3),
    Rhop = ifelse("Rhop" %in% names(fluid), fluid$Rhop, 2000),
    Rhof = ifelse("Rhof" %in% names(fluid), fluid$Rhof, 1.2),
    Croh = ifelse("Croh" %in% names(fluid), fluid$Croh, 0.05)
  )
  
  cons.bound <- list(
    E = ifelse("E" %in% names(cons.bound), cons.bound$E, 0.9),
    deltaP = ifelse("deltaP" %in% names(cons.bound), cons.bound$deltaP, 1500),
    geom.1 = ifelse("geom.1" %in% names(cons.bound), cons.bound$geom.1, 1),
    geom.2 = ifelse("geom.2" %in% names(cons.bound), cons.bound$geom.2, 0.5)
  )

  #intervals <- intervals * ratio.cut

  xmin  <- intervals[-length(intervals)]
  xmax  <- intervals[-1]
  xmean <- (xmax + xmin) / 2

  objs <- calculation_barth_muschelknautz(cyclone, fluid, xmean, delta)

  geom.cons.1 <- (Be - (Da - Dt) / 2) * cons.bound$geom.1
  geom.cons.2 <- cons.bound$geom.2 - 4 * He * Be / (pi * Dt ^ 2)
  geom.cons.3 <- 4 * He * Be / (pi * Dt ^ 2) - 0.735
  geom.cons.4 <- 1.25 * He - Ht
  geom.cons.5 <- 0.23 * Dt * (Da / (He * Be)) ^ (1 / 3) - H + Ht
  geom.cons <- c(geom.cons.1, geom.cons.2, geom.cons.3, geom.cons.4, geom.cons.5)
  objs.cons <- objs - c(cons.bounds$E, cons.bound$deltaP)
  
  values <- c(1 - objs[1], objs[2] / cons.bound$deltaP, geom.cons, -objs.cons[1], objs.cons[2] / cons.bound$deltaP)
  
  return(values)
}

#' Cyclone Simulation: Barth/Muschelknautz
#'
#' Calculate cyclone collection efficiency according to Barth/Muschelknautz.
#'
#' @param cyclone vector of cyclone's geometrical parameters (Da, Dt, H, Ht, He, Be)
#' @param fluid list of fluid parameters
#' @param xmean vector of middle points for the intervals of the particle size distribution
#' @param delta vector, amount of dust (percentage value) in each interval
#'
#' @return a vector (-collection efficiency, pressure drop)
#'
calculation_barth_muschelknautz <- function(cyclone, fluid, xmean, delta){

  # Cyclone gemoetry
  ra <- cyclone[1] / 2 				# cyclone radius [m]
  ri <- cyclone[2] / 2  			# vortex finder radius [m]
  h  <- cyclone[3]        	  # total cyclone height [m]
  ht <- cyclone[4]
  he <- cyclone[5]       		  # inlet section height [m]
  be <- cyclone[6]       		  # inlet section width  [m]

  # Fluid parameters
  vp   <- fluid$Vp
  croh <- fluid$Croh
  rhof <- fluid$Rhof
  rhop <- fluid$Rhop
  mu   <- fluid$Mu
  lambdag <- fluid$lambdag

  BE <- be / ra
  re <- ra - be / 2
  Fe <- be * he
  Fi <- pi * ri ^ 2
  Ff <- Fe / Fi

  # Calculation
  B <- croh / rhof
  lambda <- lambdag * (1 + 2 * sqrt(B))
  alpha <- 1.0 - (0.54 - 0.153 / Ff) * BE ^ (1 / 3)
  vi <- (vp / (pi * ri ^ 2))
  vr <- (vp / (2 * pi * ri * (h - ht)))
  U <- 1 / (Ff * alpha * ri / re + lambda * h / ri)
  vphii <- U * vi

  xGr <- (sqrt((18 * mu * vr * ri) / ((rhop - rhof) * vphii ^ 2)))
  Tf <-  function(x) (1 + 2 / (x / xGr) ^ (3.564)) ^ -1.235
  xi2 <- U ^ 2 * ri / ra * (1 - lambda * (h / ri) * U) ^ -1
  xi3 <- 2 + 3 * U ^ (4 / 3) + U ^ 2
  deltaP <- rhof / 2 * vi ^ 2 * (xi2 + xi3)
  Ew <- sum(Tf(xmean) * delta)

  x503 <- xmean[which(cumsum(delta) >= 0.5) [1]]
  ve <- vp / Fe
  vphia <- ve * (re / ra) * (1 / alpha)
  BGr <- lambda * mu * sqrt(ra * ri) / ((1 - ri / ra) * rhop * x503 ^ 2 * sqrt(vphia * vphii))
  E <- ifelse(B > BGr, 1 - BGr / B + BGr * Ew / B, Ew)

  return(c(E, deltaP))
}
