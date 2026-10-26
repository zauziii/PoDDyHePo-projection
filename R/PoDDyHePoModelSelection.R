#' Compare BIC-selected projection formulas across completed data sets
#'
#' @param imp A `mids` object, completed data frame, or list of completed frames.
#' @param DV One outcome column name.
#' @param NsVar Optional numeric predictor names modelled with natural splines.
#' @param df Candidate spline degrees of freedom, applied to each `NsVar`.
#'   The linear specification is always compared. Ignored when `knots` is given.
#' @param knots Interior knots: a vector for one spline variable, a list (one
#'   vector per variable), or one numeric knot per variable.
#' @param b.knots Boundary knots: a pair for one variable, or a list of pairs.
#'   Defaults to each variable's range in the first completed data set.
#' @param f.var Character vector of mandatory terms, for example
#'   `c("year", "sex", "age", "year:sex")`. Main effects involved in mandatory
#'   terms are retained as well.
#' @param predictors Optional candidate predictor names. Defaults to all other
#'   columns; exclude identifiers and non-predictors explicitly.
#' @param family `"auto"`, `"binomial"`, `"gaussian"` or `"multinomial"`.
#'   Auto uses binomial for two-level categorical or numeric 0/1 outcomes,
#'   multinomial for factors with more levels, and Gaussian for other numeric
#'   outcomes. Ordered factors are treated as multinomial, not ordinal logits.
#' @param interactions Whether to consider all two-way predictor interactions.
#' @param direction Stepwise search direction.
#' @param trace Whether to print the stepwise search.
#' @return A list retaining the four original elements: `Models` contains a
#'   `mira` of search results per spline specification; `Mean BIC` contains
#'   mean BICs of its common selected formula; `Selected Models` contains those
#'   formulas; `Suggested Model` is the formula with smallest mean BIC.
#'   Extra elements `Candidate BIC`, `Selection frequency` and `Family` expose
#'   comparisons and search stability. Each candidate formula is refitted to
#'   every completed data set before comparison.
#' @details This is a descriptive model-selection heuristic, not a pooled BIC
#'   test or a substitute for sensitivity analysis. Stepwise search does not
#'   guarantee a global optimum. Spline knots and numeric scaling constants are
#'   fixed from the first completed data set and embedded in formulas, making
#'   the same formula usable in subsequent projection. Numeric predictors are
#'   scaled inline for multinomial models to improve numerical stability.
#' @export
#' @examples
#' d <- testdata[!is.na(testdata$smo), c("smo", "year", "sex", "age")]
#' fit <- PoDDyHePoModelSelection(d, "smo", f.var = c("year", "age"),
#'                              interactions = FALSE)
#' fit$`Suggested Model`
PoDDyHePoModelSelection <- function(imp, DV, NsVar = NULL, df = NULL,
                                   knots = NULL, b.knots = NULL, f.var = NULL,
                                   predictors = NULL,
                                   family = c("auto", "binomial", "gaussian", "multinomial"),
                                   interactions = TRUE,
                                   direction = c("both", "backward", "forward"),
                                   trace = FALSE) {
  datasets <- .completed_data(imp)
  first <- datasets[[1L]]
  .check_name(DV, first, "DV")
  .check_flag(interactions, "interactions")
  .check_flag(trace, "trace")
  family <- match.arg(family)
  direction <- match.arg(direction)
  if (is.null(predictors)) predictors <- setdiff(names(first), DV)
  if (!is.character(predictors) || anyNA(predictors) || anyDuplicated(predictors) ||
      any(!predictors %in% setdiff(names(first), DV))) {
    stop("`predictors` must contain unique predictor column names excluding `DV`.", call. = FALSE)
  }
  used <- c(DV, predictors)
  if (any(vapply(datasets, function(d) anyNA(d[used]), logical(1)))) {
    stop("Complete outcome and candidate predictors before selecting models.", call. = FALSE)
  }
  for (variable in used) {
    types <- vapply(datasets, function(d) .column_type(d[[variable]]), character(1))
    if (any(types == "unsupported") || any(types != types[1L])) {
      stop("Model variable types must match and be supported: ", variable, ".", call. = FALSE)
    }
    if (types[1L] == "numeric" && any(vapply(datasets, function(d)
        any(!is.finite(d[[variable]])), logical(1)))) {
      stop("Numeric model values must be finite: ", variable, ".", call. = FALSE)
    }
    if (types[1L] == "factor" && any(vapply(datasets, function(d)
        !identical(levels(d[[variable]]), levels(first[[variable]])), logical(1)))) {
      stop("Inconsistent factor levels for ", variable, ".", call. = FALSE)
    }
  }
  for (variable in predictors) {
    if (any(vapply(datasets, function(d) length(unique(d[[variable]])) < 2L, logical(1)))) {
      stop("Constant candidate predictor: ", variable, ". Exclude it from `predictors`.", call. = FALSE)
    }
  }
  if (length(unique(first[[DV]])) < 2L) stop("Outcome needs at least two observed values.", call. = FALSE)
  if (family == "auto") {
    y <- first[[DV]]
    family <- if (is.factor(y) || is.character(y) || is.logical(y)) {
      if (length(unique(y)) == 2L) "binomial" else "multinomial"
    } else if (is.numeric(y) && all(y %in% c(0, 1))) "binomial" else "gaussian"
  }
  categorical <- is.factor(first[[DV]]) || is.character(first[[DV]]) || is.logical(first[[DV]])
  if (family == "gaussian" && !is.numeric(first[[DV]])) {
    stop("Gaussian outcomes must be numeric.", call. = FALSE)
  }
  response_levels <- if (categorical) .levels_present(droplevels_if_factor(first[[DV]])) else
    if (family == "multinomial") as.character(sort(unique(first[[DV]]))) else NULL
  if (family == "binomial" && !categorical && !all(first[[DV]] %in% c(0, 1))) {
    stop("Numeric binomial outcomes must be coded 0/1; use a factor for other labels.", call. = FALSE)
  }
  if (family == "binomial" && categorical && length(response_levels) != 2L) {
    stop("Binomial models require exactly two outcome categories.", call. = FALSE)
  }
  datasets <- lapply(datasets, function(d) {
    if (family %in% c("binomial", "multinomial") && !is.null(response_levels)) {
      d[[DV]] <- factor(as.character(d[[DV]]), levels = response_levels)
      if (anyNA(d[[DV]]) || any(table(d[[DV]]) == 0L)) {
        stop("All completed data sets must contain the same outcome categories.", call. = FALSE)
      }
    }
    d
  })
  first <- datasets[[1L]]
  if (!is.null(NsVar) && (!is.character(NsVar) || !length(NsVar) || anyNA(NsVar) ||
      anyDuplicated(NsVar) || any(!NsVar %in% predictors) ||
      !all(vapply(first[NsVar], is.numeric, logical(1))))) {
    stop("`NsVar` must contain unique numeric candidate predictors.", call. = FALSE)
  }
  if (!is.null(f.var) && (!is.character(f.var) || anyNA(f.var) || any(!nzchar(f.var)))) {
    stop("`f.var` must be a character vector of formula terms.", call. = FALSE)
  }
  if (is.null(NsVar) && (!is.null(df) || !is.null(knots) || !is.null(b.knots))) {
    warning("Spline settings ignored because `NsVar` is NULL.", call. = FALSE)
  }
  configurations <- list(Linear = NULL)
  if (!is.null(NsVar)) {
    if (is.null(knots) && is.null(df)) stop("Specify spline `df` or `knots`.", call. = FALSE)
    if (!is.null(knots) && !is.null(df)) warning("`df` ignored because `knots` is supplied.", call. = FALSE)
    if (is.null(knots)) {
      if (!is.numeric(df) || !length(df) || anyNA(df) || any(!is.finite(df)) ||
          any(df < 1) || any(df != floor(df)) || anyDuplicated(df)) {
        stop("`df` must contain distinct positive integer degrees of freedom.", call. = FALSE)
      }
      for (k in df) configurations[[paste0("DF=", k)]] <- list(df = k)
    } else configurations$Knots <- list(knots = .spline_list(knots, length(NsVar), "knots"))
  }
  boundaries <- if (!is.null(NsVar) && !is.null(b.knots))
    .spline_list(b.knots, length(NsVar), "b.knots", boundary = TRUE) else NULL
  models <- selected <- mean_bic <- list()
  scores <- frequencies <- list()
  for (configuration in names(configurations)) {
    replacement <- stats::setNames(lapply(predictors, as.name), predictors)
    # Embed scaling in the formula so mice sees the same numerical predictors.
    if (family == "multinomial") {
      for (variable in predictors[vapply(first[predictors], is.numeric, logical(1))]) {
        s <- stats::sd(first[[variable]])
        if (!is.finite(s) || s <= 0) stop("Constant numeric predictor: ", variable, call. = FALSE)
        replacement[[variable]] <- substitute(I((X - A) / B),
          list(X = as.name(variable), A = mean(first[[variable]]), B = s))
      }
    }
    setting <- configurations[[configuration]]
    if (!is.null(setting)) {
      for (j in seq_along(NsVar)) {
        variable <- NsVar[j]
        boundary <- if (is.null(boundaries)) range(first[[variable]]) else boundaries[[j]]
        if (length(boundary) != 2L || any(!is.finite(boundary)) || boundary[1L] >= boundary[2L]) {
          stop("Each boundary needs two finite, increasing values.", call. = FALSE)
        }
        args <- list(x = first[[variable]], Boundary.knots = boundary)
        if (is.null(setting$knots)) args$df <- setting$df else args$knots <- setting$knots[[j]]
        if (!is.null(args$knots) && (any(!is.finite(args$knots)) ||
            anyDuplicated(args$knots) || is.unsorted(args$knots, strictly = TRUE) ||
            any(args$knots <= boundary[1L] | args$knots >= boundary[2L]))) {
          stop("Interior knots must be finite, increasing and inside the boundaries.", call. = FALSE)
        }
        basis <- do.call(splines::ns, args)
        replacement[[variable]] <- substitute(splines::ns(X, knots = K, Boundary.knots = B),
          list(X = as.name(variable), K = attr(basis, "knots"), B = attr(basis, "Boundary.knots")))
      }
    }
    terms <- vapply(replacement, function(x) paste(deparse(x, width.cutoff = 500L, backtick = TRUE), collapse = ""), character(1))
    fixed <- character()
    if (length(f.var)) {
      for (text in f.var) {
        expr <- tryCatch(str2lang(text), error = function(e) stop("Invalid fixed term: ", text, call. = FALSE))
        vars <- all.vars(expr)
        if (any(!vars %in% predictors)) stop("Fixed terms must use candidate predictors only: ", text, call. = FALSE)
        expr <- .replace_symbols(expr, replacement)
        fixed <- c(fixed, terms[vars], paste(deparse(expr, width.cutoff = 500L, backtick = TRUE), collapse = ""))
      }
    }
    fixed <- unique(fixed)
    lower <- stats::as.formula(paste(.quote_name(DV), "~", if (length(fixed)) paste(fixed, collapse = " + ") else "1"), env = baseenv())
    rhs <- if (!length(terms)) "1" else if (interactions && length(terms) > 1L)
      paste0("(", paste(terms, collapse = " + "), ")^2") else paste(terms, collapse = " + ")
    if (length(fixed)) rhs <- paste(rhs, paste(fixed, collapse = " + "), sep = " + ")
    upper <- stats::as.formula(paste(.quote_name(DV), "~", rhs), env = baseenv())
    fits <- lapply(seq_along(datasets), function(i) {
      dat <- datasets[[i]]
      initial <- if (direction == "backward") upper else lower
      fit <- .fit_projection_model(initial, dat, family)
      if (trace) {
        fit <- stats::step(fit, scope = list(lower = lower, upper = upper),
                           direction = direction, k = log(nrow(dat)), trace = 1)
      } else {
        # Some nnet add1 methods print even when step(trace = 0) is requested.
        # Capture ordinary output while preserving warnings and errors.
        invisible(utils::capture.output(
          fit <- stats::step(fit, scope = list(lower = lower, upper = upper),
                             direction = direction, k = log(nrow(dat)), trace = 0)
        ))
      }
      .check_fit(fit, family)
      fit
    })
    models[[configuration]] <- mice::as.mira(fits)
    formulas_found <- lapply(fits, stats::formula)
    formula_text <- vapply(formulas_found, .formula_key, character(1))
    distinct <- formulas_found[!duplicated(formula_text)]
    candidate_rows <- lapply(distinct, function(f) {
      bic <- vapply(datasets, function(dat) stats::BIC(.fit_projection_model(f, dat, family)), numeric(1))
      data.frame(Specification = configuration, Formula = .formula_key(f),
                  Mean_BIC = mean(bic), SD_BIC = if (length(bic) > 1L) stats::sd(bic) else 0,
                  stringsAsFactors = FALSE)
    })
    scored <- do.call(rbind, candidate_rows)
    winner <- which.min(scored$Mean_BIC)
    selected[[configuration]] <- distinct[[winner]]
    mean_bic[[configuration]] <- scored$Mean_BIC[winner]
    scores[[configuration]] <- scored
    frequencies[[configuration]] <- data.frame(
      Specification = configuration, Formula = unique(formula_text),
      Count = as.integer(table(factor(formula_text, levels = unique(formula_text)))),
      Imputations = length(datasets), stringsAsFactors = FALSE)
  }
  best <- which.min(unlist(mean_bic))
  list(Models = models, `Mean BIC` = mean_bic, `Selected Models` = selected,
       `Suggested Model` = selected[[best]], `Candidate BIC` = do.call(rbind, scores),
       `Selection frequency` = do.call(rbind, frequencies), Family = family)
}

droplevels_if_factor <- function(x) if (is.factor(x)) droplevels(x) else x

.formula_key <- function(f) paste(deparse(f, width.cutoff = 500L, backtick = TRUE), collapse = " ")

.replace_symbols <- function(expr, mapping) {
  if (is.symbol(expr) && as.character(expr) %in% names(mapping)) return(mapping[[as.character(expr)]])
  if (is.call(expr) && length(expr) > 1L) {
    for (i in seq.int(2L, length(expr))) expr[[i]] <- .replace_symbols(expr[[i]], mapping)
  }
  expr
}

.spline_list <- function(x, n, arg, boundary = FALSE) {
  if (is.list(x)) out <- x else if (n == 1L) out <- list(x) else
    if (!boundary && is.numeric(x) && length(x) == n) out <- as.list(x) else {
      stop("`", arg, "` needs a list with one entry per spline variable.", call. = FALSE)
    }
  if (length(out) != n || !all(vapply(out, is.numeric, logical(1)))) {
    stop("`", arg, "` needs numeric values for every spline variable.", call. = FALSE)
  }
  out
}

.fit_projection_model <- function(formula, dat, family) {
  fit <- if (family == "multinomial") {
    nnet::multinom(formula, data = dat, trace = FALSE, maxit = 1000, MaxNWts = 100000)
  } else {
    stats::glm(formula, data = dat,
               family = if (family == "binomial") stats::binomial() else stats::gaussian(),
               na.action = stats::na.fail)
  }
  # Store actual data/formula in the call: step() can refit in its own context.
  fit$call$data <- dat
  fit$call$formula <- formula
  if (family != "multinomial") fit$call$family <- if (family == "binomial") stats::binomial() else stats::gaussian()
  .check_fit(fit, family)
  fit
}

.check_fit <- function(fit, family) {
  ok <- if (family == "multinomial") identical(fit$convergence, 0L) || identical(fit$convergence, 0) else isTRUE(fit$converged)
  if (!ok || !is.finite(stats::BIC(fit))) stop("Model did not converge to a finite BIC.", call. = FALSE)
  if (family == "binomial" && isTRUE(fit$boundary)) warning("Binomial fit reached a boundary; check separation.", call. = FALSE)
  invisible(fit)
}
