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

# List of CMOPs
cmops <- list(loffler = loffler20,
              stairmand = stairmand20,
              muschelknautz = muschelknautz20)
