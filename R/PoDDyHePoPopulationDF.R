#' Description of PoDDyHePoPopulationDF()
#' 
#' @title Population Forecasts Function Developed for PoDDy-HePo Project
#'
#' @description PoDDyHePoPopulationDF returns a data frame with data needs to be projected.
#'
#' @details 
#'
#' @param data Well-formatted data. 
#' @param file Population forcasts from statistic Finland.
#' @param size Size of sample to be projected
#' @param y2pred years that need to be projected.
#'
#'
#' @return a data frame
#' @export
#' 
#' @import dplyr
#' @importFrom plyr rbind.fill 
#' @importFrom magrittr %>% 
#' @importFrom utils read.table

PoDDyHePoPopulationDF <- function(data, file, size, y2pred){
  
  # read in data from statistic Finland 
  population <- utils::read.table(file, sep = ";", skip = 2, header = T, check.names = F, colClasses = c(Area="NULL"))
  
  # Get total amount from the data
  total <- population %>%
    select(grep(".*Total|Age", names(population), value = F)) %>%
    mutate(Age = suppressWarnings(as.numeric(get("Age")))) %>%
    subset(get("Age") >= range(data$age)[1] & get("Age") <= range(data$age)[2]) %>%
    arrange(get("Age"))

  # Get predicted amount for men
  male <- population %>%
    select(grep(".*Males|Age", names(population), value = F)) %>%
    mutate(Age = suppressWarnings(as.numeric(get("Age")))) %>%
    subset(get("Age") >= range(data$age)[1] & get("Age") <= range(data$age)[2]) %>%
    arrange(get("Age"))

  # Get prdicted amount for women
  female <- population %>%
    select(grep(".*Females|Age", names(population), value = F)) %>%
    mutate(Age = suppressWarnings(as.numeric(get("Age")))) %>%
    subset(get("Age") >= range(data$age)[1] & get("Age") <= range(data$age)[2]) %>%
    arrange(get("Age"))
  
  # Creat data frame for saving prop of men and women. 
  prop.m <- data.frame(male$Age)
  prop.w <- data.frame(female$Age)
  
  # Compute proportion of men and women
  for (i in 2:length(total)) {
    prop.m[, i] <- male[, i]/sum(total[, i])
    prop.w[, i] <- female[, i]/sum(total[, i])
  }

  # Rename data frames
  colnames(prop.m) <- c("Age", y2pred)
  colnames(prop.w) <- c("Age", y2pred)
  
  # select the years that we need
  prop.m <-  prop.m %>% 
    subset(select = c("Age", y2pred))
  
  prop.w <-  prop.w %>% 
    subset(select = c("Age", y2pred))

  # Size of sample to be projected
  NewN <- size

  NewSet_Total<- data.frame()
  for (i in 1:(length(y2pred))) {
    for (j in 1:nrow(prop.m)) {
      NewSet1 <- data.frame(year = rep(y2pred[i], round(prop.m[j, i+1] * NewN)), sex = rep(1, round(prop.m[j, i+1] * NewN)), age = rep(range(data$age)[1] - 1 + j, round(prop.m[j, i+1] * NewN)))
      NewSet2 <- data.frame(year = rep(y2pred[i], round(prop.w[j, i+1] * NewN)), sex = rep(2, round(prop.w[j, i+1] * NewN)), age = rep(range(data$age)[1] - 1 + j, round(prop.w[j, i+1] * NewN)))
      NewSet_Total <- rbind(NewSet_Total, NewSet1, NewSet2)
    }
  }

  # Final data frame
  p2pred <- plyr::rbind.fill(data[, names(data)],
                 cbind(NewSet_Total))

  return(p2pred)
}
  
