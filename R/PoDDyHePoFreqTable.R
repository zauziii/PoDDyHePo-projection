#' Description of PoDDyHePoFreqTable()
#' 
#' @title Frequency table Function Developed for PoDDy-Hepo Project
#'
#' @description It returns a table arranged by Year and Sex.
#'
#' @details The table includes total, respondents (N) and its proportion (%).
#'
#' @param data A data frame. It could be the well-formatted data, or the data frame whose prevalences are to be estimated.
#' @param colName A column that you want to create a frequency table for.
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
                                "Total" = ~ sum(!is.na(get("year")[which(sukup == 1)])),
                                "Response" = ~ sum(!is.na(get(colName)[which(sukup == 1)])),
                                "%" = ~ round(100 * mean(!is.na(get(colName)[which(sukup == 1)])), 1)
                              ),
                            "Female" =
                              list(
                                "Total" = ~ sum(!is.na(get("year")[which(sukup == 2)])),
                                "Response" = ~ sum(!is.na(get(colName)[which(sukup == 2)])),
                                "%" = ~ round(100 * mean(!is.na(get(colName)[which(sukup == 2)])), 1)
                              )
  )

  print(qwraps2::summary_table(
    group_by(data, get("year")),
    summary_statistic
  ))

}
    
