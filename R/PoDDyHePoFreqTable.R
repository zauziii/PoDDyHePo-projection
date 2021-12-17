#' Description of PoDDyHePoFreqTable()
#' @title Frequency table Function Developed for PoDDy-Hepo Project
#'
#' @description It returns a table arranged by Gender and Year.
#'
#' @details The table includes Total, Missing amount and its proportion (%).
#'
#' @param data A data frame. It could the well-formatted data, or the data frame whose prevalences are to be estimated.
#' @param colName A column that you want to creat a frequency table for.
#'
#'
#' @return a table
#' @export
#' 
#' @importFrom qwraps2 summary_table
#' @importFrom dplyr group_by


PoDDyHePoFreqTable <- function(data, colName){
  
  options(qwraps2_markup = "markdown")
  summary_statistic <- list("Male" =
                              list(
                                "Total" = ~ sum(!is.na(get("year")[which(sex == 1)])),
                                "Missing" = ~ sum(is.na(get(colName)[which(sex == 1)])),
                                "%" = ~ round(100 * mean(is.na(get(colName)[which(sex == 1)])), 1)
                              ),
                            "Female" =
                              list(
                                "Total" = ~ sum(!is.na(get("year")[which(sex == 2)])),
                                "Missing" = ~ sum(is.na(get(colName)[which(sex == 2)])),
                                "%" = ~ round(100 * mean(is.na(get(colName)[which(sex == 2)])), 1)
                              )
  )

  print(qwraps2::summary_table(
    group_by(data, get("year")),
    summary_statistic
  ))

}
    
