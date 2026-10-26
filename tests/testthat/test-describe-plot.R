test_that("frequency summaries use sex, return data and preserve options", {
  d <- small_survey()
  before <- options()
  out <- PoDDyHePoFreqTable(d, "y")
  expect_true(is.data.frame(out))
  expect_equal(sum(out$Total), nrow(d))
  expect_equal(sum(out$Missing), 1)
  expect_identical(options(), before)
  d$z <- c(1, NA, 1, 0, 0, NA)
  expect_equal(nrow(PoDDyHePoFreqTable(d, c("y", "z"))), 8)
  expect_equal(nrow(PoDDyHePoFreqTable(d, "y", grpVar = NULL)), 2)
  d$sex[1] <- NA
  expect_equal(sum(PoDDyHePoFreqTable(d, "y")$Total), nrow(d))
})

test_that("all four plotting layouts build with titles and diagnostics", {
  d <- data.frame(year = rep(c(2000, 2010, 2030), each = 12),
                   sex = rep(c("A", "B"), 18),
                   y = rep(c(0, 1, 1), 12),
                   category = factor(rep(0:2, 12), levels = 0:2))
  for (variable in c("y", "category")) {
    for (group in list(NULL, "sex")) {
      out <- PoDDyHePoPool(list(d, d), variable, group, diagnostics = TRUE)
      p <- PoDDyHePoPlot(out, 2010, title = "A real title", y_max = 90)
      expect_s3_class(p, "ggplot")
      expect_identical(p$labels$title, "A real title")
      expect_no_warning(ggplot2::ggplot_build(p))
      expect_equal(nrow(p$layers[[2]]$data), if (variable == "y") {
        if (is.null(group)) 2L else 4L
      } else if (is.null(group)) 6L else 12L)
    }
  }
})

test_that("plotting supports many categories, label checks and missing boundary years", {
  d <- expand.grid(year = c(2000, 2007, 2030), sex = 1:2, y = 1:9)
  d$y <- factor(d$y)
  out <- PoDDyHePoPool(d, "y", "sex")
  p <- PoDDyHePoPlot(out, 2017, c("Men", "Women"), "Nine categories")
  expect_no_warning(ggplot2::ggplot_build(p))
  expect_true(all(p$layers[[2]]$data$Year %in% c(2007, 2030)))
  expect_error(PoDDyHePoPlot(out, 2017, grplabels = "One"), "per level")
  expect_error(PoDDyHePoPlot(out, 2017, y_min = 70, y_max = 50), "limits")
})
