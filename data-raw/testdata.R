# This is a NEW synthetic example, not a reconstruction of the original data.
# Run from the package root: source("data-raw/testdata.R")
local({
  had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv) else
    rm(".Random.seed", envir = .GlobalEnv))
  set.seed(2026)
  n <- 1000L
  year <- rep(c(1997, 2002, 2007, 2012, 2017), each = n / 5)
  age <- sample(25:64, n, replace = TRUE)
  sex <- factor(sample(1:2, n, replace = TRUE), levels = 1:2)
  male <- as.integer(sex == "1")
  trend <- (year - 2007) / 10
  older <- (age - 45) / 10
  binary <- function(eta) factor(rbinom(n, 1, plogis(eta)), levels = 0:1)
  testdata <- data.frame(
    year = year, sex = sex, age = age,
    obe = factor(sample(0:2, n, replace = TRUE, prob = c(.46, .36, .18)), levels = 0:2),
    smo = binary(-1.1 - .25 * trend + .35 * male - .10 * older),
    highbp = binary(-.8 + .50 * older + .20 * male),
    edulv = factor(sample(1:3, n, replace = TRUE, prob = c(.3, .4, .3)), levels = 1:3),
    marsta = binary(.5 + .15 * older),
    area = factor(sample(2:6, n, replace = TRUE), levels = 2:6),
    dbt = binary(-2.4 + .6 * older),
    sed = binary(-.6 + .15 * older),
    highkol = binary(-.2 + .45 * older))
  for (v in setdiff(names(testdata), c("year", "sex", "age"))) {
    testdata[[v]][sample.int(n, floor(.06 * n))] <- NA
  }
  dir.create("data", showWarnings = FALSE)
  save(testdata, file = "data/testdata.rda", compress = "xz", version = 2)
})
