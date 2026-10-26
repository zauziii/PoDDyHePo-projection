test_that("pooling rejects incompatible outcome types across imputations", {
  a <- data.frame(year = rep(2000, 4), y = c(FALSE, TRUE, FALSE, TRUE))
  b <- a; b$y <- as.integer(b$y)
  expect_error(PoDDyHePoPool(list(a, b), "y"), "types.*match")
})

test_that("irrelevant row names do not change pooling", {
  a <- data.frame(year = rep(c(2000, 2010), each = 4),
                   sex = rep(c("M", "F"), 4), y = rep(c(0, 1, 1, 0), 2))
  b <- a; rownames(b) <- paste0("person", seq_len(nrow(b)))
  out <- PoDDyHePoPool(list(a, b), "y", "sex")
  expect_equal(out$EST, PoDDyHePoPool(list(a, a), "y", "sex")$EST)
})

test_that("Wilson limits enclose boundary estimates across sample sizes", {
  bad <- integer()
  for (n in seq_len(200)) {
    d <- data.frame(year = rep(2000, n), y = rep(1, n))
    out <- PoDDyHePoPool(d, "y")
    if (out$CI_LOWER > out$EST || out$CI_UPPER < out$EST) bad <- c(bad, n)
  }
  expect_equal(bad, integer(), info = paste("Non-enclosing intervals at n =", paste(bad, collapse = ",")))
  out <- PoDDyHePoPool(data.frame(year = rep(2000, 3), y = rep(1, 3)), "y")
  expect_no_error(ggplot2::ggplot_build(PoDDyHePoPlot(out, 2000)))
})

test_that("Wilson intervals agree with an independent reference for different confidence levels", {
  fixtures <- expand.grid(n = c(8, 25, 100), conf = c(.8, .9, .95, .99))
  differences <- numeric(nrow(fixtures))
  for (i in seq_len(nrow(fixtures))) {
    n <- fixtures$n[i]; k <- floor(n / 3); level <- fixtures$conf[i]
    d <- data.frame(year = rep(2000, n), y = c(rep(1, k), rep(0, n - k)))
    out <- PoDDyHePoPool(list(d, d), "y", conf.level = level)
    expected <- suppressWarnings(stats::prop.test(k, n, correct = FALSE,
                                                   conf.level = level)$conf.int)
    differences[i] <- max(abs(c(out$CI_LOWER, out$CI_UPPER) - expected))
  }
  expect_true(all(differences < 1e-12))
})

test_that("confidence levels just below one still yield finite intervals", {
  d <- data.frame(year = rep(2000, 20), y = rep(c(0, 1), 10))
  out <- PoDDyHePoPool(d, "y", conf.level = 1 - .Machine$double.eps / 2)
  expect_true(all(is.finite(c(out$CI_LOWER, out$CI_UPPER))))
  expect_lte(out$CI_LOWER, out$EST)
  expect_gte(out$CI_UPPER, out$EST)
})

test_that("dots in labels do not merge separate plotting series", {
  d <- expand.grid(year = c(2000, 2010, 2030), sex = c("A", "B"), category = 0:2)
  d$category <- factor(d$category)
  out <- PoDDyHePoPool(d, "category", "sex")
  p <- PoDDyHePoPlot(out, 2010, grplabels = c("a", "a.b"),
                      sepvarlbl = c("b.c", "c", "d"))
  expect_equal(nlevels(p$data$.series), 6)
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("plotting respects named labels and works after CSV round-trip", {
  d <- expand.grid(year = c(2000, 2030), sex = c("A", "B"), category = 0:2)
  d$category <- factor(d$category)
  out <- PoDDyHePoPool(d, "category", "sex", diagnostics = TRUE)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(out, path, row.names = FALSE)
  reread <- utils::read.csv(path)
  p <- PoDDyHePoPlot(reread, 2000, grplabels = c(B = "Second", A = "First"),
                      sepvarlbl = c(category_2 = "High", category_0 = "Low", category_1 = "Middle"))
  expect_identical(levels(p$data$.facet), c("First", "Second"))
  expect_identical(levels(p$data$.colour), c("Low", "Middle", "High"))
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("population construction preserves logical sex values", {
  d <- small_survey(); d$sex <- as.character(d$sex) == "1"
  pop <- small_population(); pop$sex <- pop$sex == "1"
  out <- PoDDyHePoPopulationDF(d, population = pop, size = 11, y2pred = 2030)
  expect_type(out$sex, "logical")
  expect_identical(out$sex[seq_len(nrow(d))], d$sex)
  expect_equal(sum(out$year == 2030), 11)
})

test_that("population construction handles sparse observed ages and custom codes", {
  d <- small_survey()
  d$age[d$age == 26] <- 27L
  d$sex <- factor(ifelse(d$sex == "1", "M", "F"), levels = c("M", "F"), ordered = TRUE)
  pop <- expand.grid(year = 2030, age = 25:27, sex = c("M", "F"), stringsAsFactors = FALSE)
  pop$population <- 1
  out <- PoDDyHePoPopulationDF(d, population = pop, size = 6, y2pred = 2030)
  expect_equal(sort(unique(out$age[out$year == 2030])), 25:27)
  expect_identical(levels(out$sex), c("M", "F"))
  expect_true(is.ordered(out$sex))
  expect_true(all(table(out$age[out$year == 2030], out$sex[out$year == 2030]) == 1))
})

test_that("CSV sex mappings and grouped ages outside the target range work", {
  d <- small_survey(); d$sex <- factor(ifelse(d$sex == "1", "M", "F"))
  wide <- data.frame(Age = c("0-4", "25", "26", "100 -"),
                      `2030 Males` = c(10, 90, 10, 8),
                      `2030 Females` = c(20, 10, 90, 12), check.names = FALSE)
  path <- tempfile(fileext = ".csv")
  utils::write.table(wide, path, sep = ";", row.names = FALSE)
  out <- PoDDyHePoPopulationDF(d, path, size = 200, y2pred = 2030,
                                sex_map = c(Males = "M", Females = "F"))
  expect_equal(sum(out$year == 2030 & out$age == 25 & out$sex == "M"), 90)
  expect_equal(sum(out$year == 2030 & out$age == 26 & out$sex == "F"), 90)
})

test_that("frequency totals agree with independently computed group counts", {
  d <- small_survey(); d$sex[1] <- NA
  out <- PoDDyHePoFreqTable(d, "y")
  for (i in seq_len(nrow(out))) {
    group <- if (is.na(out$sex[i])) is.na(d$sex) else !is.na(d$sex) & d$sex == out$sex[i]
    expected <- d[d$year == out$Year[i] & group, , drop = FALSE]
    expect_equal(out$Total[i], nrow(expected))
    expect_equal(out$Response[i], sum(!is.na(expected$y)))
  }
})

test_that("all search directions and intercept-only scopes work", {
  d <- model_data(120)
  for (direction in c("both", "forward", "backward")) {
    fit <- PoDDyHePoModelSelection(d, "y", f.var = "year", interactions = FALSE,
                                     direction = direction)
    expect_true("year" %in% attr(stats::terms(fit$`Suggested Model`), "term.labels"))
    expect_true(is.finite(min(unlist(fit$`Mean BIC`))))
  }
  intercept <- PoDDyHePoModelSelection(d, "y", predictors = character())
  expect_length(attr(stats::terms(intercept$`Suggested Model`), "term.labels"), 0)
})

test_that("inconsistent model inputs and non-finite predictors fail clearly", {
  d <- model_data(100)
  bad <- d; bad$x[2] <- Inf
  expect_error(PoDDyHePoModelSelection(bad, "y"), "finite")
  bad <- d; bad$sex <- factor(rep("M", nrow(bad)))
  expect_error(PoDDyHePoModelSelection(bad, "y"), "Constant.*sex")
  bad <- d; bad$sex <- factor(paste0("group", bad$sex))
  expect_error(PoDDyHePoModelSelection(list(d, bad), "y"), "levels.*sex")
})

test_that("multinomial selection honours the quiet trace setting", {
  d <- model_data(180); set.seed(103)
  d$y <- factor(sample(c("Normal", "Overweight", "Obese"), nrow(d), replace = TRUE))
  expect_output(fit <- PoDDyHePoModelSelection(d, "y", f.var = c("year", "x"),
                                               interactions = FALSE, trace = FALSE), NA)
  expect_equal(fit$Family, "multinomial")
})

test_that("projection accepts dot formulas and rejects self-predicting targets", {
  d <- model_data(100); d$age <- rep(25:49, 4); d$y[1:3] <- NA
  fit <- PoDDyHePoProjection(d, m = 2, maxit = 2, seed = 9, formulas = list(y ~ .))
  expect_false(anyNA(mice::complete(fit)$y))
  expect_setequal(all.vars(fit$formulas$y), names(d))
  expect_error(PoDDyHePoProjection(d, formulas = list(y ~ y + age)), "itself")
})

test_that("projection uses only declared predictors and only training donors", {
  d <- model_data(100); d$age <- rep(25:49, 4); d$y[c(3, 7, 9)] <- NA
  pop <- expand.grid(year = 2030, age = 25:49, sex = c("1", "2"), stringsAsFactors = FALSE)
  pop$population <- 100
  future <- PoDDyHePoPopulationDF(d, population = pop, size = 12, y2pred = 2030)
  future$x[future$year == 2030] <- 0
  calls <- new.env(); calls$values <- list()
  name <- "mice.impute.poddyhepoprobe"
  existed <- exists(name, .GlobalEnv, inherits = FALSE)
  if (existed) previous <- get(name, .GlobalEnv)
  on.exit(if (existed) assign(name, previous, .GlobalEnv) else rm(list = name, envir = .GlobalEnv))
  probe <- function(y, ry, x, wy, ...) {
    calls$values[[length(calls$values) + 1L]] <- list(ry = ry, columns = colnames(x))
    sample(y[ry], sum(wy), replace = TRUE)
  }
  assign(name, probe, .GlobalEnv)
  fit <- PoDDyHePoProjection(future, m = 2, maxit = 2, seed = 11,
                              method = c(y = "poddyhepoprobe"),
                              formulas = list(y ~ year + sex + age))
  expect_false(anyNA(mice::complete(fit)$y))
  expect_length(calls$values, 4)
  expect_true(all(vapply(calls$values, function(z) !"x" %in% z$columns, logical(1))))
  expect_true(all(vapply(calls$values, function(z) !any(z$ry[future$year == 2030]), logical(1))))
  expect_true(all(vapply(calls$values, function(z) sum(z$ry) == 97L, logical(1))))
})

test_that("a local seed restores an initially absent random state", {
  existed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (existed) previous <- get(".Random.seed", .GlobalEnv)
  on.exit(if (existed) assign(".Random.seed", previous, .GlobalEnv) else
    if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  if (existed) rm(".Random.seed", envir = .GlobalEnv)
  PoDDyHePoPopulationDF(small_survey(), population = small_population(), size = 9,
                         y2pred = 2030, allocation = "multinomial", seed = 8)
  expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})
