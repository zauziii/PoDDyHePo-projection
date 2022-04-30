#' Description of PoDDyHePoPool()
#' @title Pool Function Developed for PoDDy-HePo project.
#'
#' @description It calculates prevalences for variables of interest and combines the estimates from imputed data set.
#'
#' @details It is able to detect separated variable and returns a data frame with Year, Sex, estimated prevalence (EST), CI_LOWER and CI_UPPER.
#'
#' @param imp Complete data (mids object) from PoDDyHePoProjeciton(). 
#' @param colName A variable whose prevalences are to be estimated.
#' @param grpVar Grouping variable. In our case, "sex" used as grouping variable.
#' @param sep_col A variable (usually has 3 levels or more) to be calculated its proportion. 
#'
#'
#' @return Returns a data frame ready for plotting.
#' @export
#' @import MIWilson dplyr
#' @importFrom magrittr %>% 

PoDDyHePoPool <- function(imp, colName, grpVar = "sex", sep_col = NULL){
  
  if(!is.null(sep_col)){
    
    # Long format
    imp.complete <- complete(imp, action = "long", include = T) 
    
    # Calculated the proportion for each level (Create dummies) - if necessary
    imp.complete <- imp.complete %>% 
      fastDummies::dummy_cols(select_columns = sep_col, ignore_na = T)
    
    # Transform back to mids
    imp <- as.mids(imp.complete)
  }
  
  # Turn imputed data to long format
  imp.long <- complete(imp, action = "long", include = T)
  
  # Detect proportional variables, if it is, save column names
  # If it is not and not even numeric, make it numeric. 
  if(sum(grepl(paste0(".*", colName, "_"), names(imp.long))) > 2){
    colNames <- names(imp.long[, grep(paste0(".*", colName, "_"), names(imp.long))])
  } else if(!is.numeric(imp.long[[colName]])){
    imp.long[[colName]] <- as.numeric(imp.long[[colName]]) - 1
  }
  
  # Transform back to mids
  imp.New <- as.mids(imp.long)
  
  
  pool <- data.frame()
  # Compute mean and standard error in each imputed dataset
  if(sum(grepl(paste0(".*", colName, "_"), names(imp.long))) > 2){
    for (k in colNames) {
      for (i in sort(unique(imp.New$data[[grpVar]]))) {
        for (v in sort(unique(imp.New$data$year))) {
          mean <- Qbar(Qhats(filter(imp.New, get(grpVar) == i & get("year") == v), k))
          lower <- mi_wilson(filter(imp.New, get(grpVar) == i & get("year") == v), k, summaries = F)[[1]]
          upper <- mi_wilson(filter(imp.New, get(grpVar) == i & get("year") == v), k, summaries = F)[[2]]
          pool <- rbind(pool, cbind(mean, lower, upper))
        }
      }
    }
    
    # Create a data frame for plotting
    # Create Year
    pool$Year <- imp.New$data$year %>%
      unique() %>%
      sort() %>%
      rep(length(colNames)*2)
    
    # Create grouping variable
    pool$Grp <- sort(unique(imp.New$data[[grpVar]])) %>%
      rep(times = length(colNames), each = length(unique(imp.New$data$year))) %>% 
      as.factor()
    
    # Create Group, if there exist proportional columns
    pool$Group <- colNames %>% 
      rep(each = length(sort(unique(imp.New$data$year))) * 2)
    
    # Rename and Rearrange
    colnames(pool) <- c("EST", "CI_LOWER", "CI_UPPER", "Year", grpVar, "Group")
    pool <- pool[, c("Year", grpVar, "EST", "CI_LOWER", "CI_UPPER", "Group")]
    
  } else{
    
    for (i in sort(unique(imp.New$data[[grpVar]]))) {
      for (v in sort(unique(imp.New$data$year))) {
        mean <- Qbar(Qhats(filter(imp.New, get(grpVar) == i & get("year") == v), colName))
        lower <- mi_wilson(filter(imp.New, get(grpVar) == i & get("year") == v), colName, summaries = F)[[1]]
        upper <- mi_wilson(filter(imp.New, get(grpVar) == i & get("year") == v), colName, summaries = F)[[2]]
        pool <- rbind(pool, cbind(mean, lower, upper))
      }
    }
    
    # Create a data frame for plotting
    # Create Year
    pool$Year <- imp.New$data$year %>%
      unique() %>%
      sort() %>%
      rep(2)
    
    # Create grouping variable
    pool$Grp <- sort(unique(imp.New$data[[grpVar]])) %>%
      rep(times = 1, each = length(unique(imp.New$data$year))) %>% 
      as.factor()
    
    # Rename and rearrange
    colnames(pool) <- c("EST", "CI_LOWER", "CI_UPPER", "Year", grpVar)
    pool <- pool[, c("Year", grpVar, "EST", "CI_LOWER", "CI_UPPER")]
    
  }
  
  return(pool)
}