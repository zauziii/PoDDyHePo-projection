# A short, reproducible workflow. Run after installing poddyhepo.
library(poddyhepo)

data(testdata, package = "poddyhepo")
survey <- testdata[c("year", "sex", "age", "smo")]

# 1. Describe missingness. The result can be written to CSV.
response <- PoDDyHePoFreqTable(survey, "smo")
print(response)

# 2. Complete historical survey data before selecting a model.
historical <- mice::mice(survey, m = 3, maxit = 3,
                         seed = 2026, printFlag = FALSE)
selection <- PoDDyHePoModelSelection(
  historical, DV = "smo", NsVar = "year", df = 2,
  f.var = c("year", "sex", "age"), interactions = FALSE)
print(selection$`Candidate BIC`)

# 3. This is a 2019-vintage population forecast, for demonstration only.
forecast_file <- system.file("extdata", "003_128v_2040_20210730-015759.csv",
                              package = "poddyhepo")
future <- PoDDyHePoPopulationDF(
  survey, file = forecast_file, size = 400, y2pred = c(2030, 2040))

# 4. Use the selected formula; future rows do not train the imputation models.
projection <- PoDDyHePoProjection(
  future, m = 3, maxit = 3, seed = 2026,
  formulas = list(selection$`Suggested Model`))
print(projection$loggedEvents)

# 5. Inspect numerical summaries and a reusable ggplot object.
pooled <- PoDDyHePoPool(projection, "smo", "sex", diagnostics = TRUE)
print(pooled)
figure <- PoDDyHePoPlot(
  pooled, year = 2017, grplabels = c("Men", "Women"),
  title = "Current smoking: synthetic example", y_max = 60)
print(figure)
# ggplot2::ggsave("smoking_projection.png", figure, width = 7, height = 4)

# For research use, increase m/maxit, inspect convergence and sensitivity,
# and consider survey design and the uncertainty of the population forecast.
