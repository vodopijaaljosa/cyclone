### Libraries ---------------------------------------------------------------------------
source("R/cyclone.R")
source("R/optimization.R")
source("R/utilities.R")
library(ggplot2)

### Problems ----------------------------------------------------------------------------

# CMOP 1
problem.1 <- list(lower.bounds = c(0.750, 0.450, 2.350, 0.950, 0.750, 0.180),
                  upper.bounds = c(0.950, 0.560, 2.950, 1.200, 0.950, 0.224),
                  cons = c(1, 2, 3, 4, 5, 6, 7))

# CMOP 2
problem.2 <- list(lower.bounds = c(0.900, 0.475, 3.140,	1.120, 0.900, 0.180),
                  upper.bounds = c(1.120,	0.600, 3.920,	1.400, 1.120, 0.224),
                  cons = c(1, 2, 3, 4, 5, 6, 7))

# CMOP 3 (Loffler10)
problem.3 <- list(lower.bounds = c(1.134, 0.378, 2.250,	0.585, 0.540, 0.180),
                  upper.bounds = c(1.386,	0.462, 2.750,	0.715, 0.660, 0.220),
                  cons = c(1, 2, 3, 4, 5, 6, 7))

### Runs --------------------------------------------------------------------------------
pfs.1 <- opt_mo(problem.1, method = "nsga2")
make_plot(pfs.1)
pf.1 <- pfs.1[[paste0("run.", 1)]]

pfs.2 <- opt_mo(problem.2, method = "nsga2")
make_plot(pfs.2)
pf.2  <- pfs.2[[paste0("run.", 1)]]

pfs.3 <- opt_mo(problem.3, method = "nsga2")
make_plot(pfs.3)
pf.3  <- pfs.3[[paste0("run.", 1)]]

### DE ----------------------------------------------------------------------------------
res.de.1 <- opt_so(problem.1, 1)
opt.ce.1 <- abs(res.de.1$optim$bestval)
opt.dv.1 <- round(res.de.1$optim$bestmem, 3)
names(opt.dv.1) <- c("Da", "Dt", "H", "Ht", "He", "Be")

res.de.2 <- opt_so(problem.2, 1, control = list(no.iters = 300))
opt.ce.2 <- abs(res.de.2$optim$bestval)
opt.dv.2 <- round(res.de.2$optim$bestmem, 3)
names(opt.dv.2) <- c("Da", "Dt", "H", "Ht", "He", "Be")

### Random sampling ---------------------------------------------------------------------
pop.x.1 <- t(replicate(1e6, runif(6, problem.1$lower.bounds, problem.1$upper.bounds)))
pop.y.1 <- t(apply(pop.x.1, 1, fun_cyclone))

# Feasibility ratio per constriant
apply(pop.y.1[, 3:9] <= 0, 2, sum) / nrow(pop.y.1)
sum(apply(pop.y.1[, 3:9] <= 0, 1, all)) / nrow(pop.y.1)

pop.x.2 <- t(replicate(1e6, runif(6, problem.2$lower.bounds, problem.2$upper.bounds)))
pop.y.2 <- t(apply(pop.x.2, 1, fun_cyclone))

# Feasibility ratio per constriant
apply(pop.y.2[, 3:9] <= 0, 2, sum) / nrow(pop.y.2)
sum(apply(pop.y.2[, 3:9] <= 0, 1, all)) / nrow(pop.y.2)

pop.x.3 <- t(replicate(1e6, runif(6, problem.3$lower.bounds, problem.3$upper.bounds)))
pop.y.3 <- t(apply(pop.x.3, 1, fun_cyclone))

# Feasibility ratio per constriant
apply(pop.y.3[, 3:9] <= 0, 2, sum) / nrow(pop.y.3)
sum(apply(pop.y.3[, 3:9] <= 0, 1, all)) / nrow(pop.y.3)

### Plots -------------------------------------------------------------------------------
p.1 <- ggplot(pf.1, aes(x = -ce, y = pd)) +
  theme_bw() +
  ggtitle("CMOP1") +
  xlab("f1: Collection efficency") +
  ylab("f2: Pressure drop") +
  geom_vline(xintercept = opt.ce.1, color = "red", linetype = "dashed", size = 1.5) +
  scale_x_continuous(breaks = c(0.85, 0.855, 0.86, 0.865, 0.87, opt.ce), labels = c("0.850", "0.855", "0.860", "0.865", "0.870", "SO: 0.876")) +
  geom_point()

p.2 <- ggplot(pf.2, aes(x = -ce, y = pd)) +
  theme_bw() +
  ggtitle("CMOP2") +
  xlab("f1: Collection efficency") +
  ylab("f2: Pressure drop") +
  geom_vline(xintercept = opt.ce.2, color = "red", linetype = "dashed", size = 1.5) +
  scale_x_continuous(breaks = c(0.85, 0.86, 0.87, 0.88, 0.89, opt.ce.2), labels = c("0.850", "0.860", "0.870", "0.880", "0.890", "SO: 0.898")) +
  geom_point()

Rmisc::multiplot(p.1, p.2)
