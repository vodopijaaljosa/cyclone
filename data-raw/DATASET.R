### old ---------------------------------------------------------------------------------

# CMOP1
cmop1 <- list(lower.bounds = c(0.750, 0.450, 2.350, 1.650, 0.750, 0.180),
              upper.bounds = c(0.950, 0.560, 2.950, 2.300, 0.950, 0.224))

# CMOP2
cmop2 <- list(lower.bounds = c(0.900, 0.475, 3.140, 2.290, 0.900, 0.180),
              upper.bounds = c(1.120, 0.600, 3.920, 2.850, 1.120, 0.224))

# CMOP1_Dt
cmop1.dt <- list(lower.bounds = c(0.750, 0.350, 2.350, 1.650, 0.750, 0.180),
                 upper.bounds = c(0.950, 0.560, 2.950, 2.300, 0.950, 0.224))

# CMOP2_Dt
cmop2.dt <- list(lower.bounds = c(0.900, 0.400, 3.140, 2.290, 0.900, 0.180),
                 upper.bounds = c(1.120, 0.600, 3.920, 2.850, 1.120, 0.224))

# CMOP1_Ht
cmop1.ht <- list(lower.bounds = c(0.750, 0.450, 2.350, 0.950, 0.750, 0.180),
                 upper.bounds = c(0.950, 0.560, 2.950, 1.200, 0.950, 0.224))

# CMOP2_Ht
cmop2.ht <- list(lower.bounds = c(0.900, 0.475, 3.140, 1.120, 0.900, 0.180),
                 upper.bounds = c(1.120, 0.600, 3.920, 1.400, 1.120, 0.224))

# Loffler10
loffler10 <- list(lower.bounds = c(1.134, 0.378, 2.250,	0.585, 0.540, 0.180),
                  upper.bounds = c(1.386, 0.462, 2.750, 0.715, 0.660, 0.220))

# Loffler15
loffler15 <- list(lower.bounds = c(1.071, 0.357, 2.125, 0.553, 0.510, 0.170),
                  upper.bounds = c(1.449, 0.483, 2.875, 0.748, 0.690, 0.230))

### new ---------------------------------------------------------------------------------

# Loffler20
loffler20 <- list(default = c(1.260, 0.420, 2.500, 0.650, 0.600, 0.200),
                  lower.bounds = c(1.008, 0.336, 2.000,	0.520, 0.480, 0.160),
                  upper.bounds = c(1.512, 0.504, 3.000, 0.780, 0.720, 0.240))

# Stairmand20
stairmand20 <- list(default = c(0.315, 0.157, 1.265, 0.157, 0.157, 0.0627),
                    lower.bounds = c(0.252, 0.1256, 1.012,	0.1256, 0.1256, 0.05016),
                    upper.bounds = c(0.378, 0.1884, 1.518, 0.1884, 0.1884, 0.07524))

# Muschelknautz20
muschelknautz20 <- list(default = c(0.680, 0.170, 0.934, 0.311, 0.173, 0.058),
                        lower.bounds = c(0.544, 0.136, 0.7472,	0.2488, 0.1384, 0.0464),
                        upper.bounds = c(0.816, 0.204, 1.1208, 0.3732, 0.2076, 0.0696))

### Standard cyclones ----------------------------------------------------------

standard1 <- list(
  default = c(0.750, 0.450, 2.350, 0.700, 0.450, 0.180), Vp = 1.00,
  eskal = c("delta10", "delta15"), esqua = c("delta7", "delta15")
)

standard3 <- list(
  default = c(0.950, 0.560, 2.950, 0.900, 0.560, 0.224), Vp = 1.60,
  eskal = c("delta10", "delta15"), esqua = c("delta7", "delta15")
)

standard8 <- list(
  default = c(1.700, 1.000, 5.250, 1.650, 1.000, 0.400), Vp = 5.00,
  eskal = c("delta15", "delta20"), esqua = c("delta15", "delta25")
)

standard9 <- list(
  default = c(0.450, 0.236, 1.570, 0.400, 0.224, 0.090), Vp = 0.25,
  eskal = c("delta500", "delta10"), esqua = c("delta7", "delta15")
)

# Eskal

eskal <- list(
  Rhop = 2700,
  Rhof = 2.7,
  intervals = c(0, 0.9, 1.1, 1.3, 1.8, 2.6, 3.7, 5, 7.5, 11.5, 15, 21, 30, 43, 61, 87) * 1e-6,
  delta500 = c(3.63, 1.69, 1.7, 4.7, 9.84, 18.01, 22.41, 28.1, 8.52, 1.26, 0.14) / 100,
  delta10 = c(1.22, 0.49, 0.44, 0.92, 0.99, 0.81, 1.45, 15.22, 30.77, 31.51, 13.77, 2.28, 0.13) / 100,
  delta15 = c(1.75, 0.46, 0.33, 0.49, 0.41, 0.36, 0.38, 0.39, 3.64, 24.16, 45.95, 20.07, 1.61) / 100,
  delta20 = c(0.96, 0.31, 0.26, 0.45, 0.43, 0.36, 0.32, 0.68, 1.82, 10.34, 30.24, 39.78, 12.96, 1.09) / 100
)

# Esqua

### List of CMOPs --------------------------------------------------------------
cmops <- list(loffler = loffler20,
              stairmand = stairmand20,
              muschelknautz = muschelknautz20,
              standard1 = standard1,
              standard3 = standard3,
              standard8 = standard8,
              standard9 = standard9)

usethis::use_data(cmops, overwrite = TRUE)
