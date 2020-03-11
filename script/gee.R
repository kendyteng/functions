######### GEE


## 1) make GEE dataset --> only the am and the year should be included in the df
makeGeeData <- function(df, level1) {
  data <- df %>%
    pivot_longer(names_to = "name",
                 values_to = "value",
                 - c(id, year))
  
  ## id need to be numeric and ORDERED
  data$id <- as.numeric(as.factor(data$id))
  data <- data[order(data$id), ]
  
  ## outcome needs to be numeric 0 and 1?
  data$value_numeric <- as.numeric(data$value == level1)
  
  ## year --> make it centered so quantratic doesn't correlate to linear
  data$year <- as.numeric(data$year)
  data$year1 <- data$year - mean(data$year)
  data$year1sq <- data$year1^2
  
  return(data)
}


## 2) gee summary function 
gee95ci <- function(gee) {
  df <- data.frame(coef = coef(gee))
  df$se <- summary(gee)$coefficients[,"Robust S.E."]
  df$odds <- round(exp(df$coef), 3)
  df$ll <- round(exp(df$coef - df$se * qnorm(0.975)), 3)
  df$ul <- round(exp(df$coef + df$se * qnorm(0.975)), 3)
  df$ci95 <- paste0(df$odds, " (", df$ll, " to ", 
                    df$ul, ") ")
  return(df)
}

## 3) gee fitted line function
#x: fitted, y: the data used to fit 
gee_fitted_plot <- function(gee, df, level, plot_row = 2) {
  df <- na.omit(df)
  ## fitted value
  D.fitted <- data.frame(fitted = fitted(gee), 
                         name = df$name, 
                         id = df$id, 
                         year = df$year)
  ## fitted value is probability
  #amRealN <- c("Cefotaxime", "Ciprofloxacin", "Chloramphenicol", "Florfenicol",
  #             "Gentamicin", "Nalidixic acid", "Tetracycline")
  
  # data for the dots
  d.prop <- df %>%
    group_by(name, year, value) %>%
    dplyr::summarise(count = n()) %>%
    ungroup() %>%
    group_by(name, year) %>%
    mutate(prop = count/sum(count)) %>%
    complete(name, year, value) %>%
    replace(is.na(.), 0) %>%
    filter(value == !!level) %>%
    dplyr::select(-value, -count)
  
  # plot
  name_level <- levels(as.factor(df$name))
  P.fitted <- list()
  for (i in 1:length(name_level)) {
    n_level <- quo(name_level[i])
    dots <- d.prop %>%
      filter(name == !!n_level)
    P.fitted[[i]] <- D.fitted %>% 
      filter(name == !!n_level) %>%
      distinct(year, .keep_all = TRUE) %>%
      ggplot(aes(x = year, y = fitted)) +
      geom_line(size = 1) +  
      geom_point(data = dots, 
                 aes(x = year, y = prop, group = name, color = name)) +
      scale_x_continuous(name = "Year", 
                         limits = c(range(D.fitted$year)[1], range(D.fitted$year)[2]), 
                         breaks = seq(range(D.fitted$year)[1], range(D.fitted$year)[2], 2)) +
      scale_y_continuous(name = "Proportion", 
                         limits = c(0.0, 1),
                         breaks = seq(0.0, 1, 0.1)) +
      labs(title = name_level[i]) + 
      theme_bw() 
  }
  p <- ggarrange(plotlist = P.fitted, 
                 nrow = plot_row, 
                 ncol = ceiling(length(name_level)/plot_row), 
                 legend = F)
  
  return(p)
}


## 4) gee summary
geeAmSummary <- function(gee, df, level, plot_row = 2) {
  return(list(gee$working.correlation, gee95ci(gee), gee_fitted_plot(gee, df, level)))
}



## 5) pick models with best qic
best_gee <- function(df, formula, family) {
  if(family == "binomial") {
    D.qic <- data.frame()
    for (i in seq(length(formula))) {
      f <- as.formula(formula[i])
      result <- geeglm(f, data = df,  
                    family = binomial(link = "logit"), 
                    id = id, 
                    corstr = "unstructured")
      qic <- geepack::QIC(result)
      bind <- bind_cols(id = i, qic = qic[1])
      D.qic <- rbind(D.qic, bind)
    }
  }
  D.qic <- arrange(D.qic, qic)
  return(D.qic)
}


