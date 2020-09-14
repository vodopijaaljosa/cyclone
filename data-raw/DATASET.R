### Old ------------------------------------------------------------------------

# CMOP1
cmop1 <- list(
  lower.bounds = c(0.750, 0.450, 2.350, 1.650, 0.750, 0.180),
  upper.bounds = c(0.950, 0.560, 2.950, 2.300, 0.950, 0.224)
)

# CMOP2
cmop2 <- list(
  lower.bounds = c(0.900, 0.475, 3.140, 2.290, 0.900, 0.180),
  upper.bounds = c(1.120, 0.600, 3.920, 2.850, 1.120, 0.224)
)

# CMOP1_Dt
cmop1.dt <- list(
  lower.bounds = c(0.750, 0.350, 2.350, 1.650, 0.750, 0.180),
  upper.bounds = c(0.950, 0.560, 2.950, 2.300, 0.950, 0.224)
)

# CMOP2_Dt
cmop2.dt <- list(
  lower.bounds = c(0.900, 0.400, 3.140, 2.290, 0.900, 0.180),
  upper.bounds = c(1.120, 0.600, 3.920, 2.850, 1.120, 0.224)
)

# CMOP1_Ht
cmop1.ht <- list(
  lower.bounds = c(0.750, 0.450, 2.350, 0.950, 0.750, 0.180),
  upper.bounds = c(0.950, 0.560, 2.950, 1.200, 0.950, 0.224)
)

# CMOP2_Ht
cmop2.ht <- list(
  lower.bounds = c(0.900, 0.475, 3.140, 1.120, 0.900, 0.180),
  upper.bounds = c(1.120, 0.600, 3.920, 1.400, 1.120, 0.224)
)

### New ------------------------------------------------------------------------

# Stairmand
stairmand <- list(
  default = c(0.315, 0.157, 1.265, 0.157, 0.157, 0.0627)
)

# Muschelknautz
muschelknautz <- list(
  default = c(0.680, 0.170, 0.934, 0.311, 0.173, 0.058)
)

# Loffler
loffler <- list(
  default = c(1.260, 0.420, 2.500, 0.650, 0.600, 0.200)
)

# Standard cyclones
standard.1 <- list(
  default = c(0.750, 0.450, 2.350, 0.700, 0.450, 0.180),
  fluid = list(Vp = 1.00),
  eskal = c("delta10", "delta15"),
  esqua = c("delta7", "delta15"),
  name = "Standard-1"
)

standard.3 <- list(
  default = c(0.950, 0.560, 2.950, 0.900, 0.560, 0.224),
  fluid = list(Vp = 1.60),
  eskal = c("delta10", "delta15"),
  esqua = c("delta7", "delta15"),
  name = "Standard-3"
)

standard.5 <- list(
  default = c(1.18, 0.71, 3.68, 1.13, 0.71, 0.28),
  fluid = list(Vp = 2.50),
  eskal = c("delta10", "delta15"),
  esqua = c("delta7", "delta15"),
  name = "S250"
)

standard.8 <- list(
  default = c(1.700, 1.000, 5.250, 1.650, 1.000, 0.400),
  fluid = list(Vp = 5.00),
  eskal = c("delta15", "delta20"),
  esqua = c("delta15", "delta25"),
  name = "S500"
)

standard.9 <- list(
  default = c(0.450, 0.236, 1.570, 0.400, 0.224, 0.090),
  fluid = list(Vp = 0.25),
  eskal = c("delta500", "delta10"),
  esqua = c("delta7", "delta15"),
  name = "Standard-9"
)

standard.12 <- list(
  default = c(0.63, 0.335, 2.23, 0.58, 0.315, 0.125),
  fluid = list(Vp = 0.5),
  eskal = c("delta500", "delta10"),
  esqua = c("delta7", "delta15"),
  name = "S50"
)

# EMO paper --------------------------------------------------------------------

standard.050 <- list(
  default = c(0.63, 0.335, 2.23, 0.58, 0.315, 0.125),
  fluid = list(Vp = 0.5),
  eskal = c("delta500", "delta10"),
  esqua = c("delta7", "delta15"),
  type = "high",
  name = "S050"
)

standard.100 <- list(
  default = c(0.9, 0.475, 3.14, 0.85, 0.45, 0.18),
  fluid = list(Vp = 1.0),
  eskal = c("delta10", "delta15"),
  esqua = c("delta7", "delta15"),
  type = "high",
  name = "S100"
)

standard.200 <- list(
  default = c(1.06, 0.63, 3.3, 1.01, 0.63, 0.25),
  fluid = list(Vp = 2.0),
  eskal = c("delta10", "delta15"),
  esqua = c("delta7", "delta15"),
  type = "low",
  name = "S200"
)

standard.400 <- list(
  default = c(1.5, 0.9, 4.65, 1.45, 0.9, 0.355),
  fluid = list(Vp = 4.0),
  eskal = c("delta15", "delta20"),
  esqua = c("delta15", "delta25"),
  type = "low",
  name = "S400"
)

# Eskal

eskal <- list(
  Rhop = 2700,
  intervals = c(0, 0.9, 1.1, 1.3, 1.8, 2.6, 3.7, 5, 7.5, 11.5, 15, 21, 30, 43, 61, 87) * 1e-6,
  delta500 = c(3.63, 1.69, 1.7, 4.7, 9.84, 18.01, 22.41, 28.1, 8.52, 1.26, 0.14) / 100,
  delta10 = c(1.22, 0.49, 0.44, 0.92, 0.99, 0.81, 1.45, 15.22, 30.77, 31.51, 13.77, 2.28, 0.13) / 100,
  delta15 = c(1.75, 0.46, 0.33, 0.49, 0.41, 0.36, 0.38, 0.39, 3.64, 24.16, 45.95, 20.07, 1.61) / 100,
  delta20 = c(0.96, 0.31, 0.26, 0.45, 0.43, 0.36, 0.32, 0.68, 1.82, 10.34, 30.24, 39.78, 12.96, 1.09) / 100
)

### List of CMOPs --------------------------------------------------------------

probs <- list(
  standard.050 = standard.050,
  standard.100 = standard.100,
  standard.200 = standard.200,
  standard.400 = standard.400
)

usethis::use_data(probs, overwrite = TRUE)
usethis::use_data(eskal, overwrite = TRUE)
