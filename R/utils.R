.check_data <- function(data, columns = character()) {
  if (!is.data.frame(data) || !nrow(data)) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (anyDuplicated(names(data)) || anyNA(names(data)) || any(!nzchar(names(data)))) {
    stop("Column names must be unique and non-empty.", call. = FALSE)
  }
  absent <- setdiff(columns, names(data))
  if (length(absent)) stop("Missing columns: ", paste(absent, collapse = ", "), call. = FALSE)
  invisible(data)
}

.check_name <- function(x, data, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% names(data)) {
    stop("`", arg, "` must name one column in the data.", call. = FALSE)
  }
  x
}

.check_number <- function(x, arg, lower = -Inf, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < lower ||
      (integer && x != floor(x))) {
    stop("`", arg, "` must be a finite ", if (integer) "integer " else "number ",
         ">= ", lower, ".", call. = FALSE)
  }
  invisible(x)
}

.check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
}

.check_year <- function(x, arg = "year") {
  if (!is.numeric(x) || !length(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x != floor(x))) {
    stop("`", arg, "` must contain finite integer years without missing values.", call. = FALSE)
  }
}

.check_survey <- function(data) {
  .check_data(data, c("year", "sex", "age"))
  .check_year(data$year)
  if (!is.numeric(data$age) || anyNA(data$age) || any(!is.finite(data$age)) ||
      any(data$age < 0)) {
    stop("`age` must be numeric, non-negative and fully observed.", call. = FALSE)
  }
  if (.column_type(data$sex) == "unsupported" || anyNA(data$sex)) {
    stop("`sex` must be a fully observed factor, character, numeric or logical vector.", call. = FALSE)
  }
  if (is.numeric(data$sex) && any(!is.finite(data$sex))) {
    stop("Numeric `sex` codes must be finite.", call. = FALSE)
  }
  invisible(data)
}

.column_type <- function(x) {
  if (!is.null(dim(x))) return("unsupported")
  if (is.factor(x)) "factor" else if (is.logical(x)) "logical" else
    if (is.numeric(x)) "numeric" else if (is.character(x)) "character" else "unsupported"
}

.completed_data <- function(imp) {
  if (inherits(imp, "mids")) {
    out <- lapply(seq_len(imp$m), function(i) mice::complete(imp, action = i))
  } else if (is.data.frame(imp)) {
    out <- list(imp)
  } else if (is.list(imp) && length(imp) && all(vapply(imp, is.data.frame, logical(1)))) {
    out <- imp
  } else {
    stop("`imp` must be a mids object, a data frame, or a list of completed data frames.", call. = FALSE)
  }
  lapply(out, .check_data)
  first <- out[[1L]]
  if (!all(vapply(out, function(d) identical(names(d), names(first)) &&
                  nrow(d) == nrow(first), logical(1)))) {
    stop("Completed data sets must have the same columns and number of rows.", call. = FALSE)
  }
  out
}

.quote_name <- function(x) {
  vapply(x, function(s) paste(deparse(as.name(s), backtick = TRUE), collapse = ""), character(1))
}

.levels_present <- function(x) {
  if (is.factor(x)) levels(x) else if (is.numeric(x)) as.character(sort(unique(x[!is.na(x)]))) else
    sort(unique(as.character(x[!is.na(x)])))
}

.with_seed <- function(seed, code) {
  if (is.null(seed)) return(force(code))
  .check_number(seed, "seed", 0, integer = TRUE)
  if (seed > .Machine$integer.max) stop("`seed` is too large.", call. = FALSE)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  force(code)
}
