### EMO 2021 Paper plots -------------------------------------------------------
library(ggplot2)

# Preparation ------------------------------------------------------------------
prob <- probs$standard.100
filename <- "results/nsga2-mco-S100_20_pop-100_gen-100_uuid-94244101-f6fa-48c2-8df4-756378793222.RData"
eps <- 0.2

data_preparation <- function(prob, filename, eps) {
  prob <- create_cmop(prob, eps=eps)
  res <- readRDS(filename)

  var <- res$pf$x
  lb <- matrix(rep(prob$lower.bounds, nrow(var)), nrow=nrow(var), byrow=TRUE)
  ub <- matrix(rep(prob$upper.bounds, nrow(var)), nrow=nrow(var), byrow=TRUE)
  var <- (var - lb) / (ub - lb)
  var <- data.frame(var)
  names(var) <- c("Da", "Dt", "H", "Ht", "He", "Be")

  obj <- data.frame(res$pf$y)
  names(obj) <- c("CE", "PD")
  obj$CE <- 1 - obj$CE

  pf <- cbind(var, obj)
  pf <- reshape2::melt(pf, id=c("CE", "PD"))

  return(pf)
}

# Plots ------------------------------------------------------------------------
data_plot <- function(pf){
  p <- ggplot(pf, aes(x=CE, y=PD, fill = value)) +
    theme_bw() +
    geom_point(shape=21, size=2.5) +
    scale_fill_gradient(low="white", high="black") +
    xlab("f1: Collection efficency") +
    ylab("f2: Pressure drop") +
    facet_wrap(~ variable)

  return(p)
}
