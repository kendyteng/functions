#############################
#
# frequentist model selection using aic/bic
#
#############################

### 0) to prove that it stepwise might not be the best option
stepwise_all <- function(x) {
  bic_f <- tail(stepwise(x, criterion = "BIC", direction = "forward"), 1)
  bic_b <- tail(stepwise(x, criterion = "BIC", direction = "backward"), 1)
  aic_f <- tail(stepwise(x, criterion = "AIC", direction = "forward"), 1)
  aic_b <- tail(stepwise(x, criterion = "AIC", direction = "backward"), 1)
  return(list(bic_f = bic_f, bic_b = bic_b, aic_f = aic_f, aic_b = aic_b))
}


### 1) build formula of all possible combination
makeFormula <-function(y, cov_fix = NULL, cov, int_full = NULL, 
                       int_part = NULL, cov_fix_minus = NULL) {
  # Create interaction
  n <- length(cov)
  
  # All possible number combination
  Fid <- unlist(lapply(1:n, 
                       function(i) combn(1:n, i, simplify = FALSE)), 
                recursive = FALSE)
  ## formula
  # no interactions, no fixed cov, no cov_fix_minus
  if(is.null(cov_fix) & is.null(int_full) & is.null(int_full) & is.null(cov_fix_minus)) {
    formulas <- sapply(Fid, function(j)
      paste0(y, " ~ ",
            paste0(cov[j], collapse = "+")))
  }
  # no fixed cov, full interaction with all cov, no cov_fix_minus
  if(is.null(cov_fix) & !is.null(int_full) & is.null(int_part) & is.null(cov_fix_minus)) {
    formulas <- sapply(Fid, function(j)
      paste0(y, " ~ (",
            paste0(cov[j], collapse = " +"), ")* ", int_full))
  }
  # with fixed cov, no interaction, no cov_fix_minus
  if(!is.null(cov_fix) & is.null(int_full) & is.null(int_part) & is.null(cov_fix_minus)) {
    formulas <- sapply(Fid, function(j)
      paste0(y, "~",
             paste0(cov_fix, collapse = "+"), "+",
             paste0(cov[j], collapse = "+")))
  }
  # with fixed cov and cov_fix_minus, no interaction 
  if(!is.null(cov_fix) & is.null(int_full) & is.null(int_part) & !is.null(cov_fix_minus)) {
    formulas <- sapply(Fid, function(j)
      paste0(y, "~",
             paste0(cov_fix, collapse = "+"), "-",
             paste0(cov_fix_minus, collapse = "-"), "+", 
             paste0(cov[j], collapse = "+")))
  }
  # with fixed cov, with partial interaction
  if(!is.null(cov_fix) & is.null(int_full) & !is.null(int_part) & is.null(cov_fix_minus)) {
    formulas <- sapply(Fid, function(j)
      paste0(y, "~",
             paste0(cov_fix, collapse = "+"), "+(",
             paste0(cov[j], collapse = " +"), "):", int_part))
  }
  # with fixed cov and cov_fix_minus, with partial interaction
  if(!is.null(cov_fix) & is.null(int_full) & !is.null(int_part) & !is.null(cov_fix_minus)) {
    formulas <- sapply(Fid, function(j)
      paste0(y, "~",
             paste0(cov_fix, collapse = "+"), "-",
             paste0(cov_fix_minus, collapse = "-"), "+(", 
             paste0(cov[j], collapse = " +"), "):", int_part))
  }
  return(formulas)
}


### 2) give you the summary and variables of your best aic and bic model
## pick models with best ic
best_mls <- function(df, f, n = 3, aic_wt = 0.5) {
  data <- drop_na(df)
  
  # run all models
  result <- data.frame()
  for (i in 1:length(f)) {
    lm <- lm(f[i], drop_na(data))
    aic <- AIC(lm)
    bic <- BIC(lm)
    all <- bind_cols(id = i, aic = aic, bic = bic)
    result <- rbind(result, all)
  }
  
  # choose the best id by looking into the sum of the proportional difference 
  # !! need to make the sentence better!
  lowest_aic <- result[order(result$aic), ]$aic[1] 
  lowest_bic <- result[order(result$bic), ]$bic[1] 
  result$aic_dif <- (result$aic - lowest_aic)/lowest_aic
  result$bic_dif <- (result$bic - lowest_bic)/lowest_bic
  result$all_dif <- result$aic_dif*aic_wt + result$bic_dif*(1-aic_wt)
  
  winner <- result[order(result$all_dif)[seq(n)], c(1,6)]
  
  ## the variable and coeffients of the 3 best models
  lm <- list()
  for (i in seq(n)) {
    lm[[i]] <- lm_broom(df, f[winner$id[i]])
  }
  names(lm) <- paste0("win", seq(n))
  return(list(lm = lm, winner = winner))
}


## simple linear regression model with some summary
lm_broom <- function(df, formula, ...) {
  if (!require(broom)) { #this code will make sure you have the package loaded
    install.packages("broom")
    library(broom)
  }
  lm <- lm(formula, df, ...)
  summary.lm <- tidy(lm)
  var.lm <- attributes(lm$terms)$term.labels
  
  return(list(summary.lm = summary.lm, var.lm = var.lm ))
}


vif  <- function (fit) {
  if (!require(bHmisc)) { #this code will make sure you have the package loaded
    install.packages("Hmisc")
    library(Hmisc)
  }
  v <- vcov(fit, regcoef.only = TRUE)
  nam <- dimnames(v)[[1]]
  ns <- num.intercepts(fit)
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  return(v)
}




