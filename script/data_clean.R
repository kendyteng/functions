###--- Data manipulation rational
# 1) overview of the dataset(s)
# 2) inclusion
# 3) correction
# 4) exclusion
# 5) combine
# every correction and exclusion deserve a double check!!!!

###--- when you need to combine different datasets 
# --> have a look, see whether data cleaning approach would be similar for 
# all of them. 
# If yes --> can combine then first. 
# If no --> better do them seperately but have to keep in mind the other data!

## Does any variable indicate more data should have been included?

###--- DATA
df_overview <- function(x, prior) {
  #0) dim
  dim <- dim(x)
  
  #1) the meaning of each variable
  name <- names(x)
  
  #2) the class of each variable
  str(x)
  
  #3) a quick look
  head <- head(x)
  
  #4) how complete is this data: the percentage of NA
  if (!require(purrr)) { #this code will make sure you have the package loaded
    install.packages("purrr")
    library(purrr)
  }
  
  n <- dim(x)[1]
  count <- x %>%
    map_int(~sum(is.na(.))) %>%
    sort()
  
  perc <- round(count/n*100, 1)
  name_excl <- names(perc[perc > prior])
  name_incl <- names(perc[perc <= prior])
  na_perc <- list(count = count, name_incl = name_incl, name_excl = name_excl)
  
  return(list(head = head, dim = dim, name = name, na_perc = na_perc))
} 

###--- Data cleaning, filling and correcting 
#1) duplicated column/a variable has more than one column 
#   --> R, check with previous stage 
#   --> overlaps? table()
#   --> ifelse to replace
#   --> remove the moved variable 

#2) remove the columns that you are 100% sure you are not going to use
#' can use  select(-contains("XXX"))

#3) rename
#' use tolower 
#' str_remove

#4) Filling up the missing data!!!! (only the missing but not the wrong data)
# (a) identify the ones have to be filled. 
# (b) look for the source
# (c) match, join, merge, ..., 
#     *important here. A strategy of how the datasets should be "correctly" combined 
#      and checked is very important! If there is any duplicates and NA in the 
#      "keys" that you want to use for the join, have to be careful! When there are
#      more than one "key", it's better to use them as it will allow better match!

#5) Correcting the errors 
# We will need the "maximum" "correctin" formation and in order to make decision on
# what we should do with the data, such as delete the duplicates and resampling. 
# Therefore, it's super important to make sure the data are correct. 

#(a) types of oddities for a variable
## numeric
#0) correct variable type?
# str()
#1) strange pattern in the distribution --> summary stats, plot
#2) strange pattern in the missing values --> table, plot
#3) different units used --> table, plot
#4) outliers --> plot
#5) erroneous inliers --> table with other variable, plot

## categorical 
#0) correct variable type?
#1) strange pattern in the missing values --> table, plot
#2) 2 or more categories mean the same thing --> table
#3) syntax errors: (a)white spaces, (b)letter case, (c)pad strings, (d)accent
#4) Na/0/missing/na but not real NA --> table 

#(b) these two can be used to check for the oddities above. 
check_var <- function(df) {apply(df, 2, table)}

plot_var <- function(x, y) {
  
  if (!require(ggplot2)) { 
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require(ggpubr)) { 
    install.packages("ggpubr")
    library(ggpubr)
  }
    n <- dim(x)[2]
  all_plot <- list()
  
  for (i in 1:n) {
    name <- names(x)[i]
    var <- x[, i]
    if(is.numeric(var)) {
      hist <- ggplot(x, aes(x = var)) + geom_histogram() 
      box <- ggplot(x, aes(x = factor(0), y = var)) + geom_boxplot() + coord_flip()  
      all_plot[[i]] <- ggarrange(hist, box, nrow = 1, ncol = 2, labels = name)
    }
    if(is.character(var)) {
      self <- ggplot(x, aes(x = var)) + geom_bar()
      outcome <- x[, y]
      to_y <- ggplot(x, aes(x = var, y = outcome, color = var)) + geom_boxplot()
      all_plot[[i]] <- ggarrange(self, to_y, nrow = 1, ncol = 2, labels = name)
    }
    names(all_plot)[i] <- name
  }
  return(all_plot)
}


###--- Data exclusion
###%%%### whenever there is involvement of "EXCLUDING INFORMATION", make sure 
## decision is made by having the maximum information that you could have. 
## This include (a) filling the NA and correcting the errors (b) get information
## from other columns 

#1) Does any variable indicate you should exclude some rows? --> check with previous 
#   stage, if yes (need to be careful about the NA)
#   --> use filter. When using filer, it will filter the NA away...!!!!!

#2) duplicated rows, --> R, check with previous stage 
df_uniq <- function(x) {
  
  # see if there is any duplicated raw
  sum_dupi <- sum(duplicated(x))
  print(sum_dupi)
  
  # it wouldn't change the df even without duplicate
  df <- unique(x)
  return(df)
}

#3) duplicated id, pool, variable --> R, check with previous stage 
## this function help you to see the duplicats. But when handling the real data
## it's better/ok just to use the original dataset. 
var_uniq <- function(df, var) {
  if (!require(tidyverse)) { 
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  # df is a df, var is a "variable name"
  var <- df[, var]
  
  # see if there is any duplicated id, or pool
  sum_dupi <- sum(duplicated(var))
  
  if (sum_dupi == 0) return(sum_dupi)
  if (sum_dupi > 0) {
    # duplicated id/pool with count
    dupi <- data.frame(table(var)) %>% filter(Freq > 1)
    # duplicated df
    df_dupi <- df[var %in% dupi$var, ]
    
    return(list(dupi, df_dupi))
  } 
}

## --> handle duplicated id/pool
## ---> 1. are they really the same thing?  COME OUT WITH a STRETAGY TO TELL
## ---> 2. do they information that we need? If not --> look for it
## ---> 3. determine whether to include it or not
##     ###%%%### always think about whether you will find the answer before 
##               formulating the stretegy!!!

##4) remove columns
## (a) duplicated columns
## (b) columns with all NA
remove_colNA <- function(df) select_if(df, ~sum(!is.na(.)) != 0)
## (b) get the names of columns with all 0
name_col0 <- function(df) {
  if (!require(tidyverse)) { 
    install.packages("tidyverse")
    library(tidyverse)
  }
  df %>%
    select_if(is.numeric) %>%
    select_if(map_dbl(., ~sum(., na.rm = T)) == 0) %>%
    colnames()
}

# get the columns with all zero





###----- Data combine
## If you have known that you are going to combine the data, you will have to make sure
## that 1) same names mean same things! 
##      2) same things have same names
##      3) same class for conbination 

 
###--- Diagnotics
#1) go back to the previuos step to see the consistency
#2) look for information that can confirm in the data or other data/source
#3) collect additional information


## to correct --> from obs --> single variable --> > one variable



#4) related column information does not match --> R, check with previous stage 

