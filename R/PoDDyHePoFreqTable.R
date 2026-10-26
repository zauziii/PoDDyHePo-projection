#' Response and missingness by survey year and group
#'
#' @param data A data frame containing `year` and the requested columns.
#' @param colName One or more column names to summarise.
#' @param grpVar Optional grouping column. Defaults to `"sex"`; use `NULL`
#'   for overall summaries. Missing groups are retained explicitly.
#' @return A data frame with `Year`, the grouping column (if requested),
#'   `Variable`, `Total`, `Response`, `Missing` and `Response_pct`.
#' @export
#' @examples
#' PoDDyHePoFreqTable(testdata, c("smo", "obe"))
PoDDyHePoFreqTable <- function(data, colName, grpVar = "sex") {
  .check_data(data, "year")
  .check_year(data$year)
  if (!is.character(colName) || !length(colName) || anyNA(colName) ||
      anyDuplicated(colName) || any(!colName %in% names(data))) {
    stop("`colName` must contain unique column names from the data.", call. = FALSE)
  }
  if (!is.null(grpVar)) {
    .check_name(grpVar, data, "grpVar")
    if (grpVar %in% c("year", "Year", "Variable", "Total", "Response", "Missing", "Response_pct")) {
      stop("`grpVar` conflicts with an output column.", call. = FALSE)
    }
  }
  keys <- unique(data[c("year", grpVar)])
  keys <- keys[do.call(order, keys), , drop = FALSE]
  rows <- vector("list", nrow(keys) * length(colName))
  k <- 0L
  for (variable in colName) {
    for (i in seq_len(nrow(keys))) {
      take <- data$year == keys$year[i]
      if (!is.null(grpVar)) {
        group <- keys[[grpVar]][i]
        take <- take & if (is.na(group)) is.na(data[[grpVar]]) else
          (!is.na(data[[grpVar]]) & data[[grpVar]] == group)
      }
      n <- sum(take)
      response <- sum(!is.na(data[[variable]][take]))
      row <- keys[i, , drop = FALSE]
      names(row)[1L] <- "Year"
      row$Variable <- variable
      row$Total <- n
      row$Response <- response
      row$Missing <- n - response
      row$Response_pct <- round(100 * response / n, 1L)
      k <- k + 1L
      rows[[k]] <- row
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
