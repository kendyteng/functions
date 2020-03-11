### Descriptive function 
### what do we ususally need for descriptive analysis? 
# each variable has been cheched during the data_cleaning stage

## 1) number and proportion among a categorical variable
# for exmaple: the number and proportion of serotypes
numb_prop_var <- function(df, var, round = 3, perc = 0, ...) { 
  if (!require(tidyverse)) { 
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  result <- as.data.frame(table(df[, var], ...)) %>% 
    dplyr::rename(var = Var1, count = Freq) %>%
    mutate(prop = round(count/sum(count), round)) %>%
    arrange(-prop) %>%
    filter(prop >= perc)
  return(result)
}
# ... can be used for the table function. Want NA or not useNA = c("no", "ifany", "always")

## 2) number and proportion of the positive across years or places
numb_prop_var2 <- function(df, var1, var2, level = NULL, round = 3, ...) {
  if (!require(tidyverse)) { 
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  result <- as.data.frame(table(df[, var1], df[, var2])) %>% 
    group_by(Var2) %>%
    mutate(sum = sum(Freq),
           prop = round(Freq/sum(Freq), round)) %>%
    arrange(Var1, Var2) %>%
    dplyr::rename(var1 = Var1, var2 = Var2, count = Freq) %>%
    ungroup()
  if (!is.null(level)) {
    result <- filter(result, var1 %in% level)
  }
  return(result)
}
# ... can be used for the table function. Want NA or not useNA = c("no", "ifany", "always")


## 3) number and proportion of the levels of many variable in a same rational
# for example: the number and proportion of multiple am (multiple columns) that have 
# the same layout (have to have the same layout!)
numb_prop_vars <- function(df, vars) {
  if (!require(tidyverse)) { 
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  data <- data.frame()
  for (i in seq(length(vars))) {
    result <- numb_prop_var(df, vars[i]) 
    a <-cbind(vars[i], result)
    data <- bind_rows(a, data) 
  }
  names(data) <- c("var", "level", "count", "prop")
  data <- data %>%
    complete(var, level) 
  
  return(data)
}

## 4) number and proportion of the levels of many variable in a same rational across 
# years or places
# for example: the number and proportion of multiple am (multiple columns) that have 
# the same layout (have to have the same layout!) across years or places
numb_prop_vars2 <- function(df, vars, var2, level1 = NULL, level2 = NULL) {
  if (!require(tidyverse)) { 
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  n <- length(vars)
  data <- data.frame()
  for (i in seq(n)) {
    a <- numb_prop_var2(df, vars[i], var2)
    names(a) <- c("level", "var2", "count", "prop") 
    a$var1 <- vars[i]
    data <- bind_rows(a, data)
  }
  data <- data %>%
    complete(var1, level) 
  
  if(!is.null(level1)) {
    data <- filter(data, level %in% level1)
  }
  if(!is.null(level2)) {
    data <- filter(data, var2 %in% level2)
  }
  return(data)
}

## 5) 3 variables! The propotion of variable 1 that have the character of variable 2 and
## distributed on the scale of variable 3
# exmaple Proportion of S/R in common serotype over the years 
numb_prop_var3 <- function(df, var1, var2, var3, level1 = NULL, level2 = NULL, 
                           level3 = NULL, round = 3) {
  if (!require(tidyverse)) { 
    install.packages("tidyverse")
    library(tidyverse)
  }
  data <- as.data.frame(table(df[, var1], df[, var2], df[, var3])) %>%
    dplyr::rename(level = Var1, var2 = Var2, var3 = Var3, count = Freq) %>%
    group_by(var2, var3) %>%
    mutate(prop = round(count/sum(count), round)) %>%
    mutate(var1 = var1)
  if(!is.null(level1)) {
    data <- filter(data, level %in% level1)
  }
  if(!is.null(level2)) {
    data <- filter(data, var2 %in% level2)
  }
  if(!is.null(level3)) {
    data <- filter(data, var3 %in% level3)
  }
  return(data)
}


## 6) 3 variables! The propotion of multiple variable 1 that have the character of 
## variable 2 and distributed on the scale of variable 3
# exmaple Proportion of many S/R in common serotype over the years 
numb_prop_vars3 <- function(df, var1s, var2, var3, level1 = NULL, level2 = NULL, 
                           level3 = NULL, round = 3) {
  if (!require(tidyverse)) { 
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  n <- length(var1s)
  data <- data.frame()
  for (i in seq(n)) {
    a <- numb_prop_var3(df, var1s[i], var2, var3)
    data <- bind_rows(a, data)
  }
  if(!is.null(level1)) {
    data <- filter(data, level %in% level1)
  }
  if(!is.null(level2)) {
    data <- filter(data, var2 %in% level2)
  }
  if(!is.null(level3)) {
    data <- filter(data, var3 %in% level3)
  }
  return(data)
}


## 7) Summarise the count of samples by year 
## need to give the dataset and the year 

by_year <- function(df) {
  if (!require(tidyverse)) { #this code will make sure you have the package loaded
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  final <- data.frame(year = row.names(table(df$year)))
  for (i in seq(dim(df)[2]-1)) {
    name <- names(df)[i]
    result <- df %>%
      mutate(year = as.factor(year)) %>%
      filter(!is.na(df[, i])) %>%
      dplyr::select(year) %>%
      group_by(year) %>%
      dplyr::summarise(count = n()) %>%
      complete(year) %>%
      replace(is.na(.), 0) 
    names(result) <- c("year", name)
    final <- left_join(final, result, by = "year")
  }
  return(final)
}
  

## 8) plot! Many spplot with propotions!
spplotS_prop <- function(spldf, vars, spldfNA = NULL) {
  source('D:/Google drive/Statistics/R-stats/R function/my functions/functions/script/load_pakS.R')
  packages <- c("ggplot2", "RColorBrewer", "sp", "latticeExtra", "ggpubr")
  load_pakS(packages)
  
  if (is.null(spldfNA)) {
    LP <- list()
    for (i in seq(length(vars))) {
      LP[[i]] <- 
        spplot(spldf, vars[i], lwd = 0.6, 
               col.regions = colorRampPalette(c("white", "tomato2"))(18),
               colorkey = list(labels=list(cex = 0.6),  width = 0.7),
               xlim = edges[1, ], ylim = edges[2, ], 
               main = list(label = vars[i], cex = 0.7), 
               at = seq(-0.1, 1.1, 0.08)) +
        layer_(sp.polygons(spldf, fill='grey90', lwd = 0.6))
    }
  }
  if (!is.null(spldfNA)) {
    LP <- list()
    for (i in seq(length(vars))) {
      LP[[i]] <- 
        spplot(spldf, vars[i], lwd = 0.6, 
               col.regions = colorRampPalette(c("white", "darkcyan"))(18),
               colorkey = list(labels=list(cex = 0.6),  width = 0.7),
               xlim = edges[1, ], ylim = edges[2, ], 
               main = list(label = vars[i], cex = 0.7), 
               at = seq(-0.1, 1.1, 0.08)) +
        layer_(sp.polygons(spldfNA, fill='grey90', lwd = 0.6))
    }
  }
  if (length(LP) == 1) {
    plot <- LP
  }  
  if (length(LP) > 1 & length(LP) <= 10) {
    plot <- ggarrange(plotlist = LP, 
                      nrow = 2, 
                      ncol = ceiling(length(vars)/2))
  }
  if (length(LP) > 10 & length(LP) <= 15) {
    plot <- ggarrange(plotlist = LP, 
                      nrow = 3, 
                      ncol = ceiling(length(vars)/3))
  }
  if (length(LP) > 15 & length(LP) <= 24) {
    plot <- ggarrange(plotlist = LP, 
                      nrow = 4, 
                      ncol = ceiling(length(vars)/4))
  }
  if (length(LP) > 24) {
    plot <- ggarrange(plotlist = LP, 
                      nrow = 5, 
                      ncol = ceiling(length(vars)/5))
  }
  return(plot)
}


