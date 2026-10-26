#' Plot pooled prevalences and intervals
#'
#' @param data A summary data frame from [PoDDyHePoPool()].
#' @param year Last observed survey year. Observed segments are solid; future
#'   segments are dashed and connect to the last available observation.
#' @param grplabels Optional group labels, in factor-level order (otherwise
#'   sorted value order), or a named vector mapping original values to labels.
#' @param title Optional plot title.
#' @param y_min,y_max Displayed y-axis limits in percent. Values outside the
#'   viewing window remain in the data and are clipped, rather than discarded.
#' @param sepvarlbl Optional category labels, in declared category order, or a
#'   named vector mapping `Group` values to labels.
#' @param group_var Optional grouping column. Normally read from pooling
#'   metadata or inferred from non-summary columns.
#' @return A `ggplot` object which can be customised or saved with `ggsave()`.
#' @export
#' @importFrom rlang .data
#' @examples
#' d <- testdata[!is.na(testdata$smo), ]
#' pooled <- PoDDyHePoPool(d, "smo", "sex")
#' PoDDyHePoPlot(pooled, 2017, c("Men", "Women"), "Current smoking")
PoDDyHePoPlot <- function(data, year, grplabels = NULL, title = NULL,
                          y_min = 0, y_max = 100, sepvarlbl = NULL,
                          group_var = NULL) {
  .check_data(data, c("Year", "EST", "CI_LOWER", "CI_UPPER"))
  .check_number(year, "year")
  .check_number(y_min, "y_min", 0)
  .check_number(y_max, "y_max", 0)
  if (y_min >= y_max || y_max > 100) stop("Y-axis limits must satisfy 0 <= y_min < y_max <= 100.", call. = FALSE)
  .check_year(data$Year, "Year")
  summary_cols <- c("Year", "EST", "CI_LOWER", "CI_UPPER", "Group",
                     "N", "M", "SE", "DF", "Within", "Between")
  if (is.null(group_var)) group_var <- attr(data, "grouping_variable", exact = TRUE)
  if (is.null(group_var)) {
    candidates <- setdiff(names(data), summary_cols)
    if (length(candidates) > 1L) stop("Cannot infer grouping column; supply `group_var`.", call. = FALSE)
    if (length(candidates)) group_var <- candidates
  }
  if (!is.null(group_var)) .check_name(group_var, data, "group_var")
  for (name in c("EST", "CI_LOWER", "CI_UPPER")) {
    if (!is.numeric(data[[name]]) || anyNA(data[[name]]) ||
        any(!is.finite(data[[name]])) || any(data[[name]] < 0 | data[[name]] > 1)) {
      stop("Estimates and interval limits must be finite proportions between 0 and 1.", call. = FALSE)
    }
  }
  if (any(data$CI_LOWER > data$EST | data$CI_UPPER < data$EST)) {
    stop("Confidence limits must enclose their estimates.", call. = FALSE)
  }
  category <- "Group" %in% names(data)
  if (category && anyNA(data$Group)) stop("Category labels cannot be missing.", call. = FALSE)
  if (!is.null(group_var) && anyNA(data[[group_var]])) stop("Group labels cannot be missing.", call. = FALSE)
  d <- data
  d$.facet <- if (is.null(group_var)) factor("All") else .plot_labels(d[[group_var]], grplabels, "grplabels")
  if (is.null(group_var) && !is.null(grplabels)) warning("`grplabels` ignored for ungrouped data.", call. = FALSE)
  d$.colour <- if (category) .plot_labels(d$Group, sepvarlbl, "sepvarlbl") else d$.facet
  if (!category && !is.null(sepvarlbl)) warning("`sepvarlbl` ignored for a single outcome category.", call. = FALSE)
  # Numeric level IDs avoid collisions such as a/b.c and a.b/c.
  d$.series <- interaction(as.integer(d$.facet), as.integer(d$.colour), drop = TRUE)
  if (anyDuplicated(d[c("Year", ".series")])) stop("Duplicate year-series rows cannot be plotted.", call. = FALSE)
  d <- d[order(d$.series, d$Year), , drop = FALSE]
  observed <- d[d$Year <= year, , drop = FALSE]
  future <- d[d$Year > year, , drop = FALSE]
  # Bridge each series independently, even if its last observed year is earlier.
  bridges <- observed[!duplicated(observed$.series, fromLast = TRUE) &
                        observed$.series %in% future$.series, , drop = FALSE]
  projected <- rbind(bridges, future)
  line_rows <- function(x) {
    n <- table(x$.series)
    x[x$.series %in% names(n)[n >= 2L], , drop = FALSE]
  }
  observed <- line_rows(observed)
  projected <- line_rows(projected)
  palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                "#56B4E9", "#666666", "#222222")
  n <- nlevels(d$.colour)
  if (n > length(palette)) palette <- grDevices::hcl.colors(n, "Dark 3")
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$Year, y = 100 * .data$EST,
                    colour = .data$.colour, group = .data$.series)) +
    ggplot2::geom_line(data = observed, linewidth = 0.6) +
    ggplot2::geom_line(data = projected, linewidth = 0.6, linetype = "dashed") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = 100 * .data$CI_LOWER,
                                      ymax = 100 * .data$CI_UPPER),
                           width = 0.7, linewidth = 0.35) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(values = palette[seq_len(n)]) +
    ggplot2::scale_x_continuous(breaks = sort(unique(d$Year))) +
    ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
    ggplot2::labs(x = "Year", y = "Prevalence (%)", colour = NULL, title = title) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if (category && !is.null(group_var)) p <- p + ggplot2::facet_wrap(stats::as.formula("~ .facet"))
  p
}

.plot_labels <- function(x, labels, arg) {
  lev <- .levels_present(x)
  if (is.null(labels)) labels <- lev else {
    if (!is.character(labels) || anyNA(labels) || anyDuplicated(labels)) {
      stop("`", arg, "` must contain unique non-missing character labels.", call. = FALSE)
    }
    if (!is.null(names(labels))) {
      if (anyDuplicated(names(labels)) || !all(lev %in% names(labels))) {
        stop("Named `", arg, "` must cover every level.", call. = FALSE)
      }
      labels <- unname(labels[lev])
    } else if (length(labels) != length(lev)) {
      stop("`", arg, "` must have one label per level.", call. = FALSE)
    }
  }
  factor(as.character(x), levels = lev, labels = labels)
}
