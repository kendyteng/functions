### MCA summarise- basic
mca_summ <- function(mca) {
  if (!require(FactoMineR)) { 
    install.packages("FactoMineR")
    library(FactoMineR)
  }
  if (!require(factoextra)) { 
    install.packages("factoextra")
    library(factoextra)
  }
  
  L.mca <- list()
  
  # "inertia distribution plot"
  L.mca[[1]] <- fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 45))
  
  # "(squared) correlation with the dimension"
  # the axis for categorical means the squared correlation ratio between the 
  # dimension and the variable --> in one-way ANOVA, the square of the correlation 
  # ratio is equal to the % of variability of the quantitative variable explained 
  # by the qualitative variable. the axis for continuous means the squared 
  # correlation coeff between the dimension and the variable
  L.mca[[2]] <- fviz_mca_var(mca, choice = "mca.cor", 
                             repel = TRUE, # Avoid text overlapping (slow)
                             col.var = "tomato2",  col.quanti.sup = "steelblue3")
  #it's not quantitative!!!
  
  # "factor map"
  L.mca[[3]] <- fviz_mca_biplot(mca, 
                                repel = T, # Avoid text overlapping 
                                geom.ind = "point", col.ind = "steelblue3", 
                                geom.var = c("point", "text"),
                                col.var = "tomato3",  col.quali.sup = "goldenrod3")
  
  # "statistics: dimension"
  L.mca[[4]] <- as.data.frame(round(mca$eig[, 2:3], 1))
  L.mca[[4]] <- setNames(L.mca[4],  "dimention")
  
  # "statistics: squared correlation ratio"
  L.mca[[5]] <- data.frame(round(rbind(mca$var$eta2, mca$quali.sup$eta2), 2))
  L.mca[[5]] <- setNames(L.mca[5],  "correlation2")
  
  # "statistics: contribution of variable categories"
  # contribution of the variable categories (in %) to the dimensions
  L.mca[[6]] <- data.frame(round(mca$var$contrib, 1))
  L.mca[[6]] <- setNames(L.mca[6], "contribution")
  
  # "statistics: cos2 of variable categories"  
  # degree of association between variable categories and a particular axis
  L.mca[[7]] <- data.frame(round(mca$var$cos2, 2))
  L.mca[[7]] <- setNames(L.mca[7], "cos2 variable")
  
  if (!is.null(mca$quali.sup)) {
    # "statistics: cos2 of supplementary variable categories"  
    # degree of association between variable categories and a particular axis
    L.mca[[8]] <- data.frame(round(mca$quali.sup$cos2, 3))
    L.mca[[8]] <- setNames(L.mca[8], "cos2 suppl variable")
  }
  
  return(L.mca)
}


### Plot to see the individual dots categorised by a categorical variable
mca_ind_categ <- function(df, mca, var, dimN = 4) {
  plot <- list()
  
  # put the coord into the df
  dim <- paste0("dim", seq(dimN))
  for(i in seq(dimN))  df[, dim[i]] <- mca$ind$coord[, i]
  
  # some parameter
  m <- dim(df)[1]/25
  df$var1 <- df[, var]
  
  # the plot
  plot[[1]] <- df %>% 
    group_by(var1) %>% 
    filter(n() > m) %>%
    ggplot() + 
    geom_point(aes(x = dim1, y = dim2, group = var1, color = var1), size = 2) 
  
  plot[[2]] <- df %>% 
    group_by(var1) %>% 
    filter(n() > m) %>%
    ggplot() + 
    geom_point(aes(x = dim3, y = dim4, group = var1, color = var1), size = 2) 
  
  return(plot)
} 
