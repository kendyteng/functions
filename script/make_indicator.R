#### Making indicator

makeIndicator <- function(df, var) {
  # Number of level
  n <- length(table(df[, var]))
  
  # Name and Iname of the AM
  name <- row.names(table(df[, var]))
  I <- paste0("I", tolower(name))
  
  # Create indicator
  for(i in seq(n)) {
    df[, I[i]] <- 
      ifelse(df[, var] == name[i], 1, 0)
    names(df[, I[i]]) <- I[i] 
  }
  return(df)
}