test_that("all public workflow functions are exported", {
  expect_true(all(c("PoDDyHePoFreqTable", "PoDDyHePoModelSelection",
                     "PoDDyHePoPopulationDF", "PoDDyHePoProjection",
                     "PoDDyHePoPool", "PoDDyHePoPlot") %in% getNamespaceExports("poddyhepo")))
})

test_that("projection completes future rows using explicit formulas and a seed", {
  d <- model_data(120)
  d$age <- rep(25:54, 4)
  d$y[c(2, 7, 18)] <- NA
  pop <- expand.grid(year = 2030, age = 25:54, sex = c("1", "2"),
                      stringsAsFactors = FALSE)
  pop$population <- 100
  future <- PoDDyHePoPopulationDF(d[c("year", "sex", "age", "y")],
                                  size = 30, y2pred = 2030, population = pop)
  set.seed(20); before <- .Random.seed
  a <- PoDDyHePoProjection(future, m = 2, maxit = 2, seed = 123,
                            formulas = list(y ~ year + sex + age))
  expect_identical(.Random.seed, before)
  b <- PoDDyHePoProjection(future, m = 2, maxit = 2, printFlag = FALSE,
                            y ~ year + sex + age, seed = 123)
  expect_s3_class(a, "mids")
  expect_equal(a$seed, 123)
  expect_identical(mice::complete(a), mice::complete(b))
  expect_false(anyNA(mice::complete(a)$y))
  expect_true(all(a$ignore[future$year == 2030]))
  expect_false(any(a$ignore[future$year < 2030]))
  expect_equal(names(a$formulas), "y")
  expect_equal(attr(stats::terms(a$formulas$y), "term.labels"), c("year", "sex", "age"))
  expect_equal(names(a$blocks), "y")
  pooled <- PoDDyHePoPool(a, "y", "sex")
  expect_true(all(is.finite(pooled$EST)))
})

test_that("missing formulas and invalid controls produce informative errors", {
  d <- model_data(80); d$age <- 30:109
  d$y[1] <- NA; d$x[2] <- NA
  expect_error(PoDDyHePoProjection(d, formulas = list(y ~ year + sex)), "Missing formulas for: x")
  expect_error(PoDDyHePoProjection(d, formulas = list(y ~ year, y ~ sex, x ~ year)), "Duplicate")
  expect_error(PoDDyHePoProjection(d, m = 0), "m")
  expect_error(PoDDyHePoProjection(d, maxit = 0), "maxit")
  expect_error(PoDDyHePoProjection(d, formulas = list(y ~ year, x ~ year),
                                  mice.args = list(auxiliary = TRUE)), "core")
  expect_error(PoDDyHePoProjection(d, formulas = list(y ~ unknown, x ~ year)), "Unknown")
  expect_error(PoDDyHePoProjection(d, formulas = list(y ~ year, x ~ year),
                                  method = c(y = "jomoImpute", x = "pmm"),
                                  observed_until = 2010), "held-out")
})

test_that("binary and multicategory models can be projected together", {
  d <- model_data(120)
  d$age <- rep(25:54, 4)
  set.seed(391)
  d$category <- factor(sample(0:2, 120, replace = TRUE), levels = 0:2)
  d$y[1:4] <- NA
  d$category[5:8] <- NA
  pop <- expand.grid(year = 2030, age = 25:54, sex = c("1", "2"),
                      stringsAsFactors = FALSE)
  pop$population <- 100
  future <- PoDDyHePoPopulationDF(d[c("year", "sex", "age", "y", "category")],
                                  size = 30, y2pred = 2030, population = pop)
  fit <- PoDDyHePoProjection(future, m = 2, maxit = 2, seed = 18,
                              formulas = list(y ~ year + sex + age + category,
                                              category ~ year + sex + age + y))
  expect_false(anyNA(mice::complete(fit)))
  expect_equal(fit$method[c("y", "category")], c(y = "logreg", category = "polyreg"))
  out <- PoDDyHePoPool(fit, "category", "sex")
  expect_true(all(abs(tapply(out$EST, list(out$Year, out$sex), sum) - 1) < 1e-12))
})
