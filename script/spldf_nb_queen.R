## make queen nb for many spldf

spldf_nb_queen <- function(spldf, var) {
  L.spldf <- list()
  L.nb <- list()
  for (i in seq(length(var))) {
    L.spldf[[i]] <- subset(spldf, !is.na(spldf@data[, var[i]]))
    L.nb[[i]] <- poly2nb(L.spldf[[i]], queen = FALSE)
  }
  return(list(L.spldf = L.spldf, L.nb = L.nb))
}