#' Description of PoDDyHePoPool()
#' @title Pool Function Developed for PoDDy-HePo project.
#'
#' @description It calculats prevalences for variables of interest and combines the estimates from imputed data set.
#'
#' @details It is able to detect separated variable and returns a data frame with Year, Sex, estimated prevalence (EST), CI_LOWER and CI_UPPER.
#'
#' @param imp Complete data (mids object) from PoDDyHePoProjeciton(). 
#' @param colName A variable whose prevalences are to be estimated.
#'
#'
#' @return Returns a data frame ready for plotting.
#' @export
#' @import MIWilson dplyr
#' @importFrom magrittr %>% 

PoDDyHePoPool <- function(imp, colName){
  
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
      for (i in 1:2) {
        for (v in sort(unique(imp.New$data$year))) {
          mean <- Qbar(Qhats(filter(imp.New, get("sex") == i & get("year") == v), k))
          lower <- mi_wilson(filter(imp.New, get("sex") == i & get("year") == v), k, summaries = F)[[1]]
          upper <- mi_wilson(filter(imp.New, get("sex") == i & get("year") == v), k, summaries = F)[[2]]
          pool <- rbind(pool, cbind(mean, lower, upper))
        }
      }
    }
    
    # Creat a data frame for ploting
    # Creat Year
    pool$Year <- imp.New$data$year %>%
      unique() %>%
      sort() %>%
      rep(length(colNames)*2)
    
    # Create Sex
    pool$Sex <- c(1, 2) %>%
      rep(times = length(colNames), each = length(unique(imp.New$data$year))) %>% 
      as.factor()
    
    # Creat Group, if there exist proportional columns
    pool$Group <- colNames %>% 
      rep(each = length(sort(unique(imp.New$data$year))) * 2)
    
    # Rename and Rearrange
    colnames(pool) <- c("EST", "CI_LOWER", "CI_UPPER", "Year", "Sex", "Group")
    pool <- pool[, c("Year", "Sex", "EST", "CI_LOWER", "CI_UPPER", "Group")]
    
  } else{
    for (i in 1:2) {
      for (v in sort(unique(imp.New$data$year))) {
        mean <- Qbar(Qhats(filter(imp.New, get("sex") == i & get("year") == v), colName))
        lower <- mi_wilson(filter(imp.New, get("sex") == i & get("year") == v), colName, summaries = F)[[1]]
        upper <- mi_wilson(filter(imp.New, get("sex") == i & get("year") == v), colName, summaries = F)[[2]]
        pool <- rbind(pool, cbind(mean, lower, upper))
      }
    }
    
    # Creat a data frame for ploting
    # Creat Year
    pool$Year <- imp.New$data$year %>%
      unique() %>%
      sort() %>%
      rep(2)
    
    # Creat Sex
    pool$Sex <- c(1, 2) %>%
      rep(times = 1, each = length(unique(imp.New$data$year))) %>% 
      as.factor()
    
    # Rename and rearrange
    colnames(pool) <- c("EST", "CI_LOWER", "CI_UPPER", "Year", "Sex")
    pool <- pool[, c("Year", "Sex", "EST", "CI_LOWER", "CI_UPPER")]
    
  }
  
  return(pool)
  
}
      