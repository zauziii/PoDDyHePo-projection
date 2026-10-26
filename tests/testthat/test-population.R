test_that("population rows match named years, use actual ages and sum exactly", {
  d <- small_survey()
  p <- small_population()
  out <- PoDDyHePoPopulationDF(d, size = 13, y2pred = c(2040, 2030), population = p[nrow(p):1, ])
  expect_equal(as.integer(table(out$year)[c("2030", "2040")]), c(13L, 13L))
  expect_equal(out[seq_len(nrow(d)), ], d, ignore_attr = TRUE)
  expect_identical(levels(out$sex), levels(d$sex))
  expect_type(out$age, "integer")
  expect_true(all(is.na(out$y[-seq_len(nrow(d))])))
  expect_equal(sum(attr(out, "projection_rows")), 26)
  expect_equal(attr(out, "last_observed_year"), 2005)
  # Largest-remainder approximation has less than one row of cell error.
  for (yr in c(2030, 2040)) {
    sub <- p[p$year == yr, ]
    actual <- vapply(seq_len(nrow(sub)), function(i)
      sum(out$year == yr & out$age == sub$age[i] & out$sex == sub$sex[i]), integer(1))
    expect_true(all(abs(actual - 13 * sub$population / sum(sub$population)) < 1))
  }
})

test_that("multinomial allocations are reproducible and preserve RNG", {
  set.seed(42)
  before <- .Random.seed
  a <- PoDDyHePoPopulationDF(small_survey(), size = 30, y2pred = 2030,
                             population = small_population(), allocation = "multinomial", seed = 7)
  expect_identical(.Random.seed, before)
  b <- PoDDyHePoPopulationDF(small_survey(), size = 30, y2pred = 2030,
                             population = small_population(), allocation = "multinomial", seed = 7)
  expect_identical(a, b)
})

test_that("invalid forecasts fail before making misleading populations", {
  d <- small_survey(); p <- small_population()
  expect_error(PoDDyHePoPopulationDF(d, size = 0, y2pred = 2030, population = p), "size")
  expect_error(PoDDyHePoPopulationDF(d, size = 10, y2pred = 2000, population = p), "after")
  expect_error(PoDDyHePoPopulationDF(d, size = 10, y2pred = 2030, population = p[-1, ]), "Missing population cells")
  expect_error(PoDDyHePoPopulationDF(d, size = 10, y2pred = 2030, population = rbind(p, p[1, ])), "Duplicate")
  p$population <- -1
  expect_error(PoDDyHePoPopulationDF(d, size = 10, y2pred = 2030, population = p), "non-negative")
  p$population <- 0
  expect_error(PoDDyHePoPopulationDF(d, size = 10, y2pred = 2030, population = p), "positive")
})

test_that("wide exports are read by year labels and reject ambiguity", {
  f <- system.file("extdata", "003_128v_2040_20210730-015759.csv", package = "csprojections")
  out <- PoDDyHePoPopulationDF(small_survey(), f, size = 17, y2pred = c(2040, 2025))
  expect_equal(sum(out$year == 2040), 17)
  expect_equal(sum(out$year == 2025), 17)
  expect_error(PoDDyHePoPopulationDF(small_survey(), f, size = 17, y2pred = 2031), "2031")
  wide <- data.frame(Area = c("A", "A"), Age = 25:26,
                      `2030 Males` = c(90, 10), `2030 Females` = c(10, 90), check.names = FALSE)
  path <- tempfile(fileext = ".csv")
  utils::write.table(wide[c(2, 1), c(1, 2, 4, 3)], path, sep = ";", row.names = FALSE)
  out <- PoDDyHePoPopulationDF(small_survey(), path, size = 200, y2pred = 2030)
  expect_equal(sum(out$year == 2030 & out$sex == "1" & out$age == 25), 90)
  wide$Area <- c("A", "B")
  utils::write.table(wide, path, sep = ";", row.names = FALSE)
  expect_error(PoDDyHePoPopulationDF(small_survey(), path, size = 20, y2pred = 2030), "one area")
  wide$Area <- "A"; wide$Age <- c("25-29", "30")
  utils::write.table(wide, path, sep = ";", row.names = FALSE)
  expect_error(PoDDyHePoPopulationDF(small_survey(), path, size = 20, y2pred = 2030), "Grouped")
})
