small_survey <- function() {
  data.frame(year = c(2000, 2000, 2000, 2000, 2005, 2005),
             sex = factor(c(1, 2, 1, 2, 1, 2), levels = 1:2),
             age = c(25L, 26L, 26L, 25L, 25L, 26L),
             y = factor(c(0, 1, 0, 1, NA, 1), levels = 0:1))
}

small_population <- function() {
  p <- expand.grid(year = c(2030, 2040), age = 25:26, sex = c("1", "2"),
                    stringsAsFactors = FALSE)
  p$population <- ifelse(p$year == 2030, c(1, 3, 1, 3, 2, 2, 2, 2),
                          c(1, 3, 1, 3, 2, 2, 2, 2))
  p
}

model_data <- function(n = 160) {
  set.seed(872)
  x <- rnorm(n)
  year <- sample(c(2000, 2005, 2010, 2015), n, replace = TRUE)
  sex <- factor(sample(1:2, n, replace = TRUE))
  y <- factor(rbinom(n, 1, plogis(.5 * x + .03 * (year - 2005))), levels = 0:1)
  data.frame(year = year, sex = sex, x = x, y = y)
}
