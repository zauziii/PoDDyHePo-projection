#' Append future populations matched to age-sex forecasts
#'
#' @param data Survey data with fully observed numeric `year`, numeric `age`,
#'   and `sex`. Ages must be whole years.
#' @param file Optional semicolon-delimited Statistics Finland export, with an
#'   `Age` column and columns beginning with a four-digit year and `Males` or
#'   `Females`. A heading before the column names is allowed.
#' @param size Positive integer number of synthetic people per future year.
#' @param y2pred Unique years after the last survey year.
#' @param population Alternative to `file`: a data frame with `year`, `age`,
#'   `sex`, and `population`, one row per age-sex-year cell.
#' @param sex_map Named character vector mapping wide-file sex labels to the
#'   survey codes. Defaults to `c(Males = "1", Females = "2")`.
#' @param allocation `"largest_remainder"` (default) deterministically
#'   apportions exactly `size` rows per year. `"multinomial"` draws cell counts.
#' @param seed Optional seed for multinomial allocation, without changing the
#'   caller's random-number state.
#' @return The survey data followed by future rows, retaining column types.
#'   Other future variables are `NA`. Attributes `projection_rows` and
#'   `last_observed_year` identify the appended rows and survey boundary.
#' @details Every whole age between the observed minimum and maximum must be
#'   covered for every observed sex and requested year. Duplicate cells, missing
#'   years, multi-area exports and grouped ages in the requested age range are
#'   rejected. The bundled 2019-vintage forecast is for demonstration only.
#' @export
#' @examples
#' f <- system.file("extdata", "003_128v_2040_20210730-015759.csv",
#'                  package = "csprojections")
#' future <- PoDDyHePoPopulationDF(testdata, f, size = 500, y2pred = 2030)
#' table(future$year)
PoDDyHePoPopulationDF <- function(data, file = NULL, size, y2pred,
                                population = NULL,
                                sex_map = c(Males = "1", Females = "2"),
                                allocation = c("largest_remainder", "multinomial"),
                                seed = NULL) {
  .check_survey(data)
  .check_number(size, "size", 1, integer = TRUE)
  if (size > .Machine$integer.max) stop("`size` is too large.", call. = FALSE)
  .check_year(y2pred, "y2pred")
  if (anyDuplicated(y2pred) || any(y2pred <= max(data$year))) {
    stop("`y2pred` must be unique years after the last observed year.", call. = FALSE)
  }
  if (any(data$age != floor(data$age))) {
    stop("Population forecasts require survey ages in whole years.", call. = FALSE)
  }
  allocation <- match.arg(allocation)
  if (is.null(file) == is.null(population)) {
    stop("Supply exactly one of `file` or `population`.", call. = FALSE)
  }
  if (!is.null(file)) population <- .read_population(file, y2pred, sex_map,
                                                   range(data$age))
  .check_data(population, c("year", "age", "sex", "population"))
  .check_year(population$year, "population$year")
  if (!is.numeric(population$age) || anyNA(population$age) ||
      any(!is.finite(population$age)) || any(population$age < 0) ||
      any(population$age != floor(population$age)) || anyNA(population$sex)) {
    stop("Population ages must be non-negative whole years; sex cannot be missing.", call. = FALSE)
  }
  if (!is.numeric(population$population) || anyNA(population$population) ||
      any(!is.finite(population$population)) || any(population$population < 0)) {
    stop("Population counts must be finite, non-negative numbers.", call. = FALSE)
  }
  population$sex <- as.character(population$sex)
  sexes <- sort(unique(as.character(data$sex)))
  ages <- seq.int(min(data$age), max(data$age))
  population <- population[population$year %in% y2pred &
                             population$age %in% ages &
                             population$sex %in% sexes, , drop = FALSE]
  if (anyDuplicated(population[c("year", "age", "sex")])) {
    stop("Duplicate age-sex-year population cells; supply one area/scenario.", call. = FALSE)
  }
  grid <- expand.grid(age = ages, sex = sexes, year = y2pred,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid <- merge(grid, population[c("year", "age", "sex", "population")],
                by = c("year", "age", "sex"), all.x = TRUE, sort = TRUE)
  if (anyNA(grid$population)) {
    bad <- grid[is.na(grid$population), c("year", "age", "sex"), drop = FALSE]
    stop("Missing population cells (year/age/sex), e.g. ",
         paste(bad[1L, ], collapse = "/"), ".", call. = FALSE)
  }
  new <- .with_seed(seed, {
    lapply(y2pred, function(yr) {
      cells <- grid[grid$year == yr, , drop = FALSE]
      if (!is.finite(sum(cells$population)) || sum(cells$population) <= 0) {
        stop("Population total must be positive and finite in year ", yr, ".", call. = FALSE)
      }
      prob <- cells$population / sum(cells$population)
      if (allocation == "multinomial") {
        counts <- as.vector(stats::rmultinom(1L, size, prob))
      } else {
        expected <- size * prob
        counts <- floor(expected)
        remainder <- as.integer(size - sum(counts))
        if (remainder > 0L) {
          idx <- order(-(expected - counts), seq_along(counts))[seq_len(remainder)]
          counts[idx] <- counts[idx] + 1L
        }
      }
      cells[rep.int(seq_len(nrow(cells)), counts), c("year", "sex", "age"), drop = FALSE]
    })
  })
  new <- do.call(rbind, new)
  # A typed NA slice preserves factor levels, ordered factors and other columns.
  future <- data[rep(NA_integer_, nrow(new)), , drop = FALSE]
  future$year <- if (is.integer(data$year)) as.integer(new$year) else new$year
  future$age <- if (is.integer(data$age)) as.integer(new$age) else new$age
  future$sex <- if (is.factor(data$sex)) {
    factor(new$sex, levels = levels(data$sex), ordered = is.ordered(data$sex))
  } else if (is.integer(data$sex)) as.integer(new$sex) else
    if (is.numeric(data$sex)) as.numeric(new$sex) else new$sex
  out <- rbind(data, future)
  rownames(out) <- NULL
  attr(out, "projection_rows") <- seq_len(nrow(out)) > nrow(data)
  attr(out, "last_observed_year") <- max(data$year)
  out
}

.read_population <- function(file, years, sex_map, age_range) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !file.exists(file)) {
    stop("`file` must point to an existing population CSV.", call. = FALSE)
  }
  if (!is.character(sex_map) || !length(sex_map) || is.null(names(sex_map)) ||
      anyNA(sex_map) || anyNA(names(sex_map)) || any(!nzchar(names(sex_map))) ||
      anyDuplicated(names(sex_map)) || anyDuplicated(sex_map)) {
    stop("`sex_map` must be a named character vector with unique labels and codes.", call. = FALSE)
  }
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  header <- which(grepl('(^|;)"?Age"?(;|$)', lines))
  if (length(header) != 1L) stop("Could not locate a unique `Age` CSV header.", call. = FALSE)
  wide <- utils::read.table(file, sep = ";", skip = header - 1L, header = TRUE,
                            check.names = FALSE, stringsAsFactors = FALSE,
                            comment.char = "", na.strings = c("", "NA", "..", "."))
  if ("Area" %in% names(wide) && length(unique(wide$Area)) != 1L) {
    stop("Supply a population export for one area only.", call. = FALSE)
  }
  raw_age <- trimws(as.character(wide$Age))
  single <- grepl("^[0-9]+$", raw_age)
  # Aggregated tails cannot be treated as individual ages without an assumption.
  lower_age <- suppressWarnings(as.numeric(sub("^([0-9]+).*", "\\1", raw_age)))
  finite_group <- grepl("^[0-9]+\\s*-\\s*[0-9]+\\s*$", raw_age)
  upper_age <- rep(Inf, length(raw_age))
  upper_age[finite_group] <- as.numeric(sub("^[0-9]+\\s*-\\s*([0-9]+)\\s*$", "\\1", raw_age[finite_group]))
  if (any(!single & !is.na(lower_age) & lower_age <= age_range[2L] & upper_age >= age_range[1L])) {
    stop("Grouped or open-ended ages overlap the requested age range; use single-year ages.", call. = FALSE)
  }
  wide <- wide[single, , drop = FALSE]
  wide$Age <- as.numeric(raw_age[single])
  pieces <- list()
  for (yr in years) {
    for (label in names(sex_map)) {
      prefix <- paste0(yr, " ", label)
      found <- which(names(wide) == prefix | startsWith(names(wide), paste0(prefix, " ")))
      if (length(found) != 1L) {
        stop("Expected one population column for ", prefix, ".", call. = FALSE)
      }
      count <- suppressWarnings(as.numeric(as.character(wide[[found]])))
      pieces[[length(pieces) + 1L]] <- data.frame(
        year = yr, age = wide$Age, sex = unname(sex_map[[label]]), population = count)
    }
  }
  do.call(rbind, pieces)
}
