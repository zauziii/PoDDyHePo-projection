test_that("binary numeric and labelled factors use actual outcome values", {
  d <- data.frame(year = rep(2000, 10), y = c(rep(1, 3), rep(0, 7)))
  a <- PoDDyHePoPool(d, "y", diagnostics = TRUE)
  expect_equal(a$EST, .3)
  expect_equal(c(a$CI_LOWER, a$CI_UPPER),
               unname(suppressWarnings(stats::prop.test(3, 10, correct = FALSE)$conf.int)),
               ignore_attr = TRUE)
  d$y <- factor(ifelse(d$y == 1, "Yes", "No"), levels = c("No", "Yes"))
  b <- PoDDyHePoPool(d, "y")
  expect_equal(b$EST, .3)
  expect_equal(PoDDyHePoPool(d, "y", event = "No")$EST, .7)
  expect_equal(attr(b, "event"), "Yes")
  expect_error(PoDDyHePoPool(d, "y", event = "Missing"), "event")
})

test_that("zero and one prevalence retain Wilson uncertainty", {
  d <- data.frame(year = rep(2000, 20), y = rep(0, 20))
  zero <- PoDDyHePoPool(list(d, d), "y")
  expect_equal(zero$EST, 0)
  expect_equal(zero$CI_LOWER, 0)
  expect_gt(zero$CI_UPPER, 0)
  expect_equal(zero$CI_UPPER, 0.1611251581, tolerance = 1e-8)
  d$y <- 1
  one <- PoDDyHePoPool(list(d, d), "y")
  expect_equal(one$CI_UPPER, 1)
  expect_lt(one$CI_LOWER, 1)
})

test_that("multiple-imputation variances are pooled and extreme inflation is bounded", {
  make <- function(k) data.frame(year = rep(2000, 100), y = c(rep(1, k), rep(0, 100-k)))
  out <- PoDDyHePoPool(list(make(20), make(25), make(30)), "y", diagnostics = TRUE)
  expect_equal(out$EST, .25)
  expect_equal(out$Between, .0025)
  expect_equal(out$Within, mean(c(.2*.8, .25*.75, .3*.7)) / 100)
  expect_equal(out$SE^2, out$Within + (1 + 1/3) * out$Between)
  single <- PoDDyHePoPool(make(25), "y")
  expect_gt(out$CI_UPPER - out$CI_LOWER, single$CI_UPPER - single$CI_LOWER)
  expect_equal(PoDDyHePoPool(list(make(0), make(100)), "y")$CI_LOWER, 0)
  expect_equal(PoDDyHePoPool(list(make(0), make(100)), "y")$CI_UPPER, 1)
})

test_that("multicategory proportions sum to one and retain unused factor levels", {
  d <- data.frame(year = rep(c(2000, 2010), each = 6), sex = rep(c("A", "B"), 6),
                   y = factor(rep(c("A", "B", "B"), 4), levels = c("B", "A", "C")))
  a <- PoDDyHePoPool(list(d, d), "y", "sex")
  expect_equal(unname(tapply(a$EST, list(a$Year, a$sex), sum)), matrix(1, 2, 2))
  expect_identical(levels(a$Group), c("y_B", "y_A", "y_C"))
  expect_true(all(a$EST[a$Group == "y_C"] == 0))
  expect_equal(nrow(PoDDyHePoPool(list(d, d), "y")), 6)
  expect_equal(nrow(PoDDyHePoPool(list(d, d), "y", "sex", event = "A")), 4)
})

test_that("pooling rejects incomplete, mismatched and continuous input", {
  d <- data.frame(year = c(2000, 2000), sex = c("A", "B"), y = c(0, 1))
  b <- d; b$sex <- rev(b$sex)
  expect_error(PoDDyHePoPool(list(d, b), "y", "sex"), "identical")
  b <- d; b$y[1] <- NA
  expect_error(PoDDyHePoPool(b, "y"), "completed")
  b$y <- c(1.2, 3.4)
  expect_error(PoDDyHePoPool(b, "y"), "integer category")
  expect_error(PoDDyHePoPool(d, "y", conf.level = 1), "between")
})
