#' Pool binary and categorical prevalences across imputations
#'
#' @param imp A `mids` object, one completed data frame, or a list of completed
#'   data frames with the same rows and columns.
#' @param colName Outcome column. Numeric outcomes must be integer category
#'   codes with at most 20 distinct values; otherwise use a factor.
#' @param grpVar Optional, fully observed grouping column, fixed across
#'   completed data sets. Use `NULL` for overall prevalences.
#' @param conf.level Two-sided confidence level, strictly between zero and one.
#' @param event Optional category whose prevalence is wanted. For a binary
#'   factor the default is the last declared level; for numeric/logical 0/1
#'   outcomes it is 1/TRUE. With more than two levels and no event, return all
#'   categories, including unused factor levels.
#' @param diagnostics Add `N`, `M`, `SE`, `DF`, `Within` and `Between` columns.
#' @return A data frame with `Year`, optional grouping column, `EST`,
#'   `CI_LOWER`, `CI_UPPER`, and `Group` for multicategory results. Estimates
#'   and limits are proportions. Attributes record event and confidence level.
#' @details Point estimates are arithmetic means of completed-data proportions.
#'   Intervals use the MI-Wilson method of Lott and Reiter (2020), with
#'   `qt((1 + conf.level) / 2, df)` for two-sided coverage. With one data set or
#'   zero between-imputation variance, the interval reduces to ordinary Wilson.
#'   An infinite variance-inflation factor gives the limiting interval
#'   \eqn{[0, 1]}.
#'
#'   These are unweighted binomial approximations. They do not account for a
#'   complex survey design, population-forecast uncertainty, model selection,
#'   or unmodelled future changes. For synthetic future rows the chosen `size`
#'   affects the within-imputation term; these are not fully calibrated future
#'   prediction intervals. Compare sizes and model assumptions explicitly.
#' @references Lott A, Reiter JP (2020). Wilson Confidence Intervals for Binomial
#'   Proportions With Multiple Imputation for Missing Data. The American
#'   Statistician, 74(2), 109-115. doi:10.1080/00031305.2018.1473796.
#' @export
#' @examples
#' d <- testdata[!is.na(testdata$smo), ]
#' pooled <- PoDDyHePoPool(d, "smo", "sex", diagnostics = TRUE)
#' head(pooled)
PoDDyHePoPool <- function(imp, colName, grpVar = NULL, conf.level = 0.95,
                          event = NULL, diagnostics = FALSE) {
  datasets <- .completed_data(imp)
  data <- datasets[[1L]]
  .check_data(data, "year")
  .check_name(colName, data, "colName")
  .check_number(conf.level, "conf.level", 0)
  if (conf.level <= 0 || conf.level >= 1) stop("`conf.level` must be between 0 and 1.", call. = FALSE)
  .check_flag(diagnostics, "diagnostics")
  if (!is.null(grpVar)) {
    .check_name(grpVar, data, "grpVar")
    if (grpVar %in% c("year", "Year", "EST", "CI_LOWER", "CI_UPPER", "Group",
                       "N", "M", "SE", "DF", "Within", "Between")) {
      stop("`grpVar` conflicts with a summary column.", call. = FALSE)
    }
  }
  keys <- c("year", grpVar)
  for (d in datasets) {
    .check_year(d$year)
    if (anyNA(d[c(keys, colName)])) {
      stop("Outcome, year and grouping values must be completed before pooling.", call. = FALSE)
    }
    if (!all(vapply(keys, function(key) identical(d[[key]], data[[key]]), logical(1)))) {
      stop("Year and grouping columns must be identical across completed data sets.", call. = FALSE)
    }
  }
  x <- data[[colName]]
  types <- vapply(datasets, function(d) .column_type(d[[colName]]), character(1))
  if (any(types != types[1L])) {
    stop("Outcome types must match across completed data sets.", call. = FALSE)
  }
  if (types[1L] == "unsupported") {
    stop("Outcome must contain binary or categorical values.", call. = FALSE)
  }
  values <- unlist(lapply(datasets, function(d) as.character(d[[colName]])), use.names = FALSE)
  if (is.factor(x)) {
    categories <- levels(x)
    if (any(!values %in% categories)) stop("Inconsistent outcome levels across imputations.", call. = FALSE)
    if (!all(vapply(datasets, function(d) identical(levels(d[[colName]]), categories), logical(1)))) {
      stop("Factor levels must match across completed data sets.", call. = FALSE)
    }
  } else if (is.numeric(x)) {
    numeric_values <- suppressWarnings(as.numeric(values))
    if (any(!is.finite(numeric_values)) || any(numeric_values != floor(numeric_values)) ||
        length(unique(values)) > 20L) {
      stop("Numeric outcomes must be integer category codes (<=20); use a factor for categories.", call. = FALSE)
    }
    categories <- as.character(sort(unique(numeric_values)))
    if (all(numeric_values %in% c(0, 1))) categories <- c("0", "1")
  } else if (is.logical(x)) {
    categories <- c("FALSE", "TRUE")
  } else categories <- sort(unique(values))
  if (!length(categories)) stop("No outcome categories found.", call. = FALSE)
  if (!is.null(event)) {
    if (length(event) != 1L || is.na(event) || !as.character(event) %in% categories) {
      stop("`event` must identify one outcome category.", call. = FALSE)
    }
    selected <- as.character(event)
  } else if (length(categories) <= 2L) selected <- utils::tail(categories, 1L) else selected <- categories
  multicategory <- is.null(event) && length(categories) > 2L
  cells <- unique(data[keys])
  cells <- cells[do.call(order, cells), , drop = FALSE]
  out <- vector("list", nrow(cells) * length(selected))
  index <- 0L
  for (category in selected) {
    for (i in seq_len(nrow(cells))) {
      take <- data$year == cells$year[i]
      if (!is.null(grpVar)) take <- take & data[[grpVar]] == cells[[grpVar]][i]
      n <- sum(take)
      q <- vapply(datasets, function(d) mean(as.character(d[[colName]][take]) == category), numeric(1))
      summary <- .mi_wilson(q, n, conf.level)
      row <- cells[i, , drop = FALSE]
      names(row)[1L] <- "Year"
      row$EST <- mean(q)
      row$CI_LOWER <- summary$ci[1L]
      row$CI_UPPER <- summary$ci[2L]
      if (multicategory) row$Group <- paste0(colName, "_", category)
      if (diagnostics) {
        row$N <- n
        row$M <- length(q)
        row$SE <- sqrt(summary$total)
        row$DF <- summary$df
        row$Within <- summary$within
        row$Between <- summary$between
      }
      index <- index + 1L
      out[[index]] <- row
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  if (multicategory) out$Group <- factor(out$Group, levels = paste0(colName, "_", categories))
  attr(out, "grouping_variable") <- grpVar
  attr(out, "outcome") <- colName
  attr(out, "event") <- if (multicategory) NULL else selected
  attr(out, "conf.level") <- conf.level
  attr(out, "ci_method") <- "MI-Wilson (two-sided)"
  out
}

# Algebra follows the MI-Wilson implementation by Frances Hung (2021), MIT.
# See inst/NOTICE for attribution. The two-sided quantile and limiting cases
# are corrected here rather than depending on the archived package.
.mi_wilson <- function(q, n, conf.level) {
  m <- length(q)
  p <- mean(q)
  within <- mean(q * (1 - q) / n)
  between <- if (m > 1L) stats::var(q) else 0
  inflation <- if (between == 0) 0 else if (within == 0) Inf else
    (1 + 1 / m) * between / within
  df <- if (inflation == 0) Inf else (m - 1) * (1 + 1 / inflation)^2
  total <- within + (1 + 1 / m) * between
  if (is.infinite(inflation)) {
    ci <- c(0, 1)
  } else {
    # Upper-tail form avoids rounding (1 + conf.level) / 2 up to exactly 1.
    critical <- stats::qt((1 - conf.level) / 2, df, lower.tail = FALSE)
    a <- critical^2 * (1 + inflation) / n
    center <- (p + a / 2) / (1 + a)
    half <- sqrt(a * p * (1 - p) + a^2 / 4) / (1 + a)
    # Guard floating-point roundoff, including p = 1 with upper < 1 by 1 ulp.
    ci <- c(min(p, max(0, center - half)), max(p, min(1, center + half)))
  }
  list(ci = ci, within = within, between = between, total = total, df = df)
}
