#' Project individual values using chained-equations imputation
#'
#' @param data Survey rows followed by future rows, normally from
#'   [PoDDyHePoPopulationDF()]. Demographic columns must be fully observed.
#' @param m Positive integer number of imputations.
#' @param maxit Positive integer number of iterations.
#' @param printFlag Whether to print iteration history.
#' @param ... Two-sided formulas, one per variable with missing values.
#' @param formulas Alternative named or unnamed list of formulas; do not also
#'   supply formulas in `...`.
#' @param seed Optional integer random seed.
#' @param method Optional named vector of univariate mice imputation methods,
#'   one per incomplete variable. Defaults to measurement-level-specific methods.
#' @param observed_until Optional last observed year. Otherwise read from the
#'   population-data attribute. Rows after this year are imputed but excluded
#'   from fitting imputation models.
#' @param mice.args Named list of additional mice arguments. Core arguments,
#'   blocks, predictor matrices and auxiliary predictors cannot be overridden.
#' @return A `mice::mids` object. Inspect its `loggedEvents` and chain traces
#'   before interpreting projections.
#' @details All incomplete variables need explicit formulas. Formulas are used
#'   exactly as supplied: auxiliary predictors are disabled. Character columns
#'   must be converted to factors. Multivariate imputation methods that ignore
#'   held-out rows are rejected when future rows are present.
#' @export
#' @examples
#' d <- testdata[c("year", "sex", "age", "smo")]
#' imp <- PoDDyHePoProjection(d, m = 2, maxit = 2,
#'                           formulas = list(smo ~ year + sex + age), seed = 42)
PoDDyHePoProjection <- function(data, m = 5, maxit = 10, printFlag = FALSE, ...,
                               formulas = NULL, seed = NULL, method = NULL,
                               observed_until = NULL, mice.args = list()) {
  .check_survey(data)
  .check_number(m, "m", 1, integer = TRUE)
  .check_number(maxit, "maxit", 1, integer = TRUE)
  .check_flag(printFlag, "printFlag")
  chars <- names(data)[vapply(data, is.character, logical(1))]
  if (length(chars)) stop("Convert character columns to factors first: ",
                          paste(chars, collapse = ", "), call. = FALSE)
  targets <- names(data)[vapply(data, anyNA, logical(1))]
  if (!length(targets)) stop("No missing values to impute or project.", call. = FALSE)
  if (any(vapply(data[targets], function(x) all(is.na(x)), logical(1)))) {
    stop("Every incomplete variable needs observed training values.", call. = FALSE)
  }
  dots <- list(...)
  if (length(dots) && !is.null(formulas)) {
    stop("Supply formulas in `...` or `formulas`, not both.", call. = FALSE)
  }
  if (is.null(formulas)) formulas <- dots
  if (inherits(formulas, "formula")) formulas <- list(formulas)
  if (!is.list(formulas) || !length(formulas) ||
      !all(vapply(formulas, inherits, logical(1), "formula"))) {
    stop("Supply one two-sided formula for each incomplete variable.", call. = FALSE)
  }
  lhs <- vapply(formulas, function(f) {
    if (length(f) != 3L || !is.symbol(f[[2L]])) {
      stop("Each formula must have one untransformed response column.", call. = FALSE)
    }
    as.character(f[[2L]])
  }, character(1))
  if (anyDuplicated(lhs)) stop("Duplicate response formulas.", call. = FALSE)
  if (!is.null(names(formulas)) && any(nzchar(names(formulas)) & names(formulas) != lhs)) {
    stop("Formula list names must match their response columns.", call. = FALSE)
  }
  absent <- setdiff(targets, lhs)
  extra <- setdiff(lhs, targets)
  if (length(absent)) stop("Missing formulas for: ", paste(absent, collapse = ", "), call. = FALSE)
  if (length(extra)) stop("Formulas supplied for fully observed or unknown columns: ",
                          paste(extra, collapse = ", "), call. = FALSE)
  names(formulas) <- lhs
  formulas <- formulas[targets]
  for (target in targets) {
    unknown <- setdiff(all.vars(formulas[[target]]), names(data))
    if (length(unknown)) stop("Unknown formula variables: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (is.null(observed_until)) observed_until <- attr(data, "last_observed_year", exact = TRUE)
  ignore <- rep(FALSE, nrow(data))
  if (!is.null(observed_until)) {
    .check_number(observed_until, "observed_until")
    ignore <- data$year > observed_until
    if (all(ignore)) stop("No observed training rows before `observed_until`.", call. = FALSE)
    for (target in targets) {
      if (all(is.na(data[[target]][!ignore]))) {
        stop("No observed training values for `", target, "`.", call. = FALSE)
      }
    }
  }
  if (is.null(method)) {
    method <- mice::make.method(data)[targets]
  } else {
    if (!is.character(method) || is.null(names(method)) || anyDuplicated(names(method)) ||
        !setequal(names(method), targets) || anyNA(method) || any(!nzchar(method))) {
      stop("`method` must name one non-empty method per incomplete variable.", call. = FALSE)
    }
    method <- method[targets]
  }
  if (any(ignore) && any(grepl("jomo|panImpute", method, ignore.case = TRUE))) {
    stop("These multivariate methods do not honour held-out rows; use univariate methods.", call. = FALSE)
  }
  if (!is.list(mice.args) || (length(mice.args) &&
      (is.null(names(mice.args)) || anyNA(names(mice.args)) ||
       any(!nzchar(names(mice.args))) || anyDuplicated(names(mice.args))))) {
    stop("`mice.args` must be a list with unique non-empty names.", call. = FALSE)
  }
  reserved <- c("data", "m", "maxit", "printFlag", "formulas", "blocks", "method",
                "seed", "ignore", "predictorMatrix", "auxiliary", "calltype", "modeltype")
  if (any(names(mice.args) %in% reserved)) {
    stop("Cannot override core mice arguments through `mice.args`: ",
         paste(intersect(names(mice.args), reserved), collapse = ", "), call. = FALSE)
  }
  # Let mice derive blocks from the named formulas. auxiliary = FALSE prevents
  # its default predictor matrix from adding terms outside those formulas.
  args <- c(list(data = data, m = m, maxit = maxit, printFlag = printFlag,
                 formulas = formulas, method = method,
                 auxiliary = FALSE, ignore = ignore,
                 seed = if (is.null(seed)) NA else seed), mice.args)
  # Seeding is scoped so a reproducible projection does not reset a user's RNG.
  imp <- .with_seed(seed, do.call(mice::mice, args))
  remaining <- vapply(seq_len(m), function(i) {
    anyNA(mice::complete(imp, action = i)[targets])
  }, logical(1))
  if (any(remaining)) {
    stop("Imputation left missing target values. Check methods, formulas and logged events.", call. = FALSE)
  }
  attr(imp, "last_observed_year") <- observed_until
  imp
}
