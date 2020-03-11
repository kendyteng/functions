## to do a bunch of Bayesian local smoothing
## 1) same spldf and nb
local_smooth1 <- function(spldf, obs, pop, nb) {
  if (!require(spdep)) { 
    install.packages("spdep")
    library(spdep)
  }
  obs_ebl <- paste0(obs, "_ebl")
  for (i in seq(length(obs))) {
    result <- EBlocal(ri = spldf@data[, obs[i]], 
                      ni = spldf@data[, pop[i]],
                      nb = nb, zero.policy = T)
    spldf@data[, obs_ebl[i]] <- result[, 2]
  }
  return(spldf)
}

## 2) different spldf and nb
local_smooth2 <- function(spldf, obs, pop, nb) {
  if (!require(spdep)) { 
    install.packages("spdep")
    library(spdep)
  }
  obs_ebl <- paste0(obs, "_ebl")
  for (i in seq(length(obs))) {
    result <- EBlocal(ri = spldf[[i]]@data[, obs[i]], 
                      ni = spldf[[i]]@data[, pop[i]],
                      nb = nb[[i]], zero.policy = T)
    spldf[[i]]@data[, obs_ebl[i]] <- result[, 2]
  }
  return(spldf)
}
