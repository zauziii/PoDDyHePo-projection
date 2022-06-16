#' Description of PoDDyHePoPool()
#' @title Pool Function Developed for PoDDy-HePo project.
#'
#' @description It calculates prevalences for variables of interest and combines the estimates from imputed data set.
#'
#' @details It is able to detect separated variable and returns a data frame with Year, Sex, estimated prevalence (EST), CI_LOWER and CI_UPPER.
#'
#' @param imp Complete data (mids object) from PoDDyHePoProjeciton(). 
#' @param colName A variable whose prevalences are to be estimated.
#' @param grpVar Grouping variable. The default is NULL. In our case, "sex" used as grouping variable, that is grpVar = "sex".
#'
#' @return Returns a data frame ready for plotting.
#' @export
#' @import MIWilson dplyr
#' @importFrom fastDummies dummy_cols
#' @importFrom magrittr %>% 

PoDDyHePoPool <- function(imp, colName, grpVar = NULL){
  
  imp.long <- complete(imp, action = "long", include = T) 
  
  # Detect proportional variables, if it is, save column names
  #  and calculate the proportions.
  # If it is not and not even numeric, make it numerical. 
  if(max(as.numeric(imp.long[[colName]]) - 1, na.rm = T) > 1){
    sep_col <- colName
    imp.long <- imp.long %>% 
      fastDummies::dummy_cols(select_columns = sep_col, ignore_na = T)
    colNames <- names(imp.long[, grep(paste0(".*", colName, "_"), names(imp.long))])
  } else if(!is.numeric(imp.long[[colName]])){
    imp.long[[colName]] <- as.numeric(imp.long[[colName]]) - 1
    sep_col <- NULL
  }
  
  # Transform back to mids
  imp.New <- as.mids(imp.long)
  
  # Create data frame, ready to collect the results
  pool <- data.frame()
  
  # Compute mean and standard error in each imputed dataset
  if(is.null(grpVar) & !is.null(sep_col)){
    # Compute mean and standard error in each imputed dataset
    for (k in colNames) {
      for (v in sort(unique(imp.New$data$year))) {
        mean <- Qbar(Qhats(filter(imp.New, get("year") == v), k))
        lower <- mi_wilson(filter(imp.New, get("year") == v), k, summaries = F)[[1]]
        upper <- mi_wilson(filter(imp.New, get("year") == v), k, summaries = F)[[2]]
        pool <- rbind(pool, cbind(mean, lower, upper))
      }
    }
    
    # Create a data frame for plotting
    # Create Year
    pool$Year <- imp.New$data$year %>%
      unique() %>%
      sort() %>%
      rep(length(colNames) * 1)
    
    # Create Group, if there exist proportional columns
    pool$Group <- colNames %>% 
      rep(each = length(sort(unique(imp.New$data$year))))
    
    # Rename and rearrange columns
    colnames(pool) <- c("EST", "CI_LOWER", "CI_UPPER", "Year", "Group")
    pool <- pool[, c("Year", "EST", "CI_LOWER", "CI_UPPER", "Group")]
    
  } else if(is.null(grpVar) & is.null(sep_col)){
    
    for (v in sort(unique(imp.New$data$year))) {
      mean <- Qbar(Qhats(filter(imp.New, get("year") == v), colName))
      lower <- mi_wilson(filter(imp.New, get("year") == v), colName, summaries = F)[[1]]
      upper <- mi_wilson(filter(imp.New, get("year") == v), colName, summaries = F)[[2]]
      pool <- rbind(pool, cbind(mean, lower, upper))
    }
    
    # Create a data frame for plotting
    # Create Year
    pool$Year <- imp.New$data$year %>%
      unique() %>%
      sort() %>%
      rep(1)
    
    # Rename and rearrange columns
    colnames(pool) <- c("EST", "CI_LOWER", "CI_UPPER", "Year")
    pool <- pool[, c("Year", "EST", "CI_LOWER", "CI_UPPER")]
    
  } else if(!is.null(grpVar) & !is.null(sep_col)){
    
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
      rep(length(colNames) * length(unique(imp.New$data[[grpVar]])))
    
    # Create grouping variable
    pool$Grp <- sort(unique(imp.New$data[[grpVar]])) %>%
      rep(times = length(colNames), each = length(unique(imp.New$data$year))) %>% 
      as.factor()
    
    # Create Group, if there exist proportional columns
    pool$Group <- colNames %>% 
      rep(each = length(sort(unique(imp.New$data$year))) * length(unique(imp.New$data[[grpVar]])))
    
    # Rename and Rearrange
    colnames(pool) <- c("EST", "CI_LOWER", "CI_UPPER", "Year", grpVar, "Group")
    pool <- pool[, c("Year", grpVar, "EST", "CI_LOWER", "CI_UPPER", "Group")]
    
  } else{ # !is.null(grpVar) & is.null(sep_col)
    
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
      rep(length(unique(imp.New$data[[grpVar]])))
    
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
