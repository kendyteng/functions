bind_LD <- function(x) {
  # create a data to put the data
  df_bind <- data.frame()
  
  # for each [[]] in the list
  for (i in 1:length(x)) {
    df <- x[[i]]
    df$df_name <- names(x[i])
    df_bind <- bind_rows(df_bind, df)
  }
  
  return(df_bind)
}
