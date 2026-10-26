test_that("linear selection works and keeps mandatory main effects and interactions", {
  d <- model_data()
  fit <- PoDDyHePoModelSelection(list(d, d), "y", f.var = "year:sex", interactions = TRUE)
  expect_equal(fit$Family, "binomial")
  expect_named(fit[1:4], c("Models", "Mean BIC", "Selected Models", "Suggested Model"))
  terms <- attr(stats::terms(fit$`Suggested Model`), "term.labels")
  expect_true(all(c("year", "sex", "year:sex") %in% terms))
  expect_true(all(fit$`Selection frequency`$Imputations == 2))
  # Check against a separate fit of the actual recommended formula.
  expected <- stats::BIC(stats::glm(fit$`Suggested Model`, data = d, family = binomial()))
  expect_equal(unname(fit$`Mean BIC`$Linear), unname(expected))
})

test_that("every candidate is rescored on the same collection of imputations", {
  d <- model_data(200); b <- d
  b$y <- factor(as.integer(seq_len(nrow(b)) %% 3 == 0), levels = 0:1)
  result <- PoDDyHePoModelSelection(list(d, b), "y", predictors = c("x", "year"), interactions = FALSE)
  scores <- result$`Candidate BIC`
  for (i in seq_len(nrow(scores))) {
    f <- stats::as.formula(scores$Formula[i])
    expected <- mean(vapply(list(d, b), function(dat)
      stats::BIC(stats::glm(f, data = dat, family = binomial())), numeric(1)))
    expect_equal(scores$Mean_BIC[i], expected)
  }
})

test_that("multiple spline variables and alternative df values are usable formulas", {
  d <- model_data(160)
  fit <- PoDDyHePoModelSelection(d, "y", NsVar = c("year", "x"),
                                  knots = list(2005, 0), b.knots = list(c(1999, 2016), c(-5, 5)),
                                  f.var = c("year", "x", "year:sex"), interactions = FALSE)
  expect_length(fit$`Selected Models`, 2)
  ns_formula <- fit$`Selected Models`$Knots
  expect_match(paste(deparse(ns_formula), collapse = " "), "splines::ns")
  expect_true(is.finite(stats::BIC(stats::glm(ns_formula, data = d, family = binomial()))))
  df_fit <- PoDDyHePoModelSelection(d, "y", NsVar = "year", df = c(2, 3),
                                     f.var = "year", interactions = FALSE)
  expect_named(df_fit$`Selected Models`, c("Linear", "DF=2", "DF=3"))
  expect_error(PoDDyHePoModelSelection(d, "y", NsVar = "year"), "df.*knots")
  expect_error(PoDDyHePoModelSelection(d, "y", NsVar = "year", knots = 2005,
                                       b.knots = c(2015, 2000)), "boundary")
})

test_that("model families match binary, multi-level and continuous outcomes", {
  d <- model_data(180)
  set.seed(103)
  d$y <- factor(sample(c("Normal", "Overweight", "Obese"), nrow(d), replace = TRUE))
  fit <- PoDDyHePoModelSelection(d, "y", f.var = c("year", "x"), interactions = FALSE)
  expect_equal(fit$Family, "multinomial")
  expect_true(all(vapply(fit$Models$Linear$analyses, inherits, logical(1), "multinom")))
  expect_true(is.finite(fit$`Mean BIC`$Linear))
  expect_match(paste(deparse(fit$`Suggested Model`), collapse = " "), "I\\(")
  d$y <- .5 * d$x + rnorm(nrow(d))
  gaussian <- PoDDyHePoModelSelection(d, "y", interactions = FALSE)
  expect_equal(gaussian$Family, "gaussian")
  expect_error(PoDDyHePoModelSelection(d, "y", family = "binomial"), "coded 0/1")
})

test_that("non-syntactic columns work without string replacement errors", {
  d <- model_data(100)
  names(d)[names(d) == "x"] <- "risk score"
  fit <- PoDDyHePoModelSelection(d, "y", predictors = "risk score", interactions = FALSE,
                                  f.var = "`risk score`")
  expect_true("risk score" %in% all.vars(fit$`Suggested Model`))
  expect_error(PoDDyHePoModelSelection(d, "y", f.var = "yearly"), "candidate predictors")
})
