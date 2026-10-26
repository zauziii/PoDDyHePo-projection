# csprojections

Project health indicator prevalences from repeated cross-sectional surveys and
age-sex population forecasts using chained-equations multiple imputation.

This **1.1.0 development upgrade** retains the six `PoDDyHePo*` function names
and repairs the workflow in the supplied 1.0.0 source snapshot. It has not been
published to CRAN or pushed to GitHub. Read `NEWS.md` before comparing results
with the earlier version: model selection, population allocation, factor
handling and confidence intervals have intentional corrections.

## Install the supplied source archive

In R, first install the dependencies, then choose the supplied `.tar.gz` file:

```r
install.packages(c("mice", "ggplot2", "nnet", "rlang"))
install.packages(file.choose(), repos = NULL, type = "source")
library(csprojections)
```

Alternatively, unpack the developer ZIP, open a terminal in its parent folder,
and run `R CMD INSTALL csprojections`. The package contains no compiled code.
R >= 4.1.0 and ggplot2 >= 3.4.0 are required. There is no need to attach
`splines`, `dplyr`, or `mice` to use the six functions.

## A complete example

The supplied snapshot did not contain the original `testdata.rda`. The bundled
`testdata` is a **new synthetic demonstration**, generated with seed 2026 from
`data-raw/testdata.R`. It contains no real participant data and cannot reproduce
the published PoDDy-HePo estimates. The bundled population CSV is the original
2019-vintage example forecast, not an updated official forecast.

```r
library(csprojections)
data(testdata, package = "csprojections")
survey <- testdata[c("year", "sex", "age", "smo")]

# 1. Check response and missingness.
PoDDyHePoFreqTable(survey, "smo")

# 2. Complete historical data, then compare linear and spline models.
historical <- mice::mice(
  survey, m = 3, maxit = 3, seed = 2026, printFlag = FALSE
)
selection <- PoDDyHePoModelSelection(
  historical, DV = "smo", NsVar = "year", df = 2,
  f.var = c("year", "sex", "age"), interactions = FALSE
)
selection$`Candidate BIC`
selection$`Selection frequency`

# 3. Append exactly 400 synthetic people per future year.
forecast_file <- system.file(
  "extdata", "003_128v_2040_20210730-015759.csv", package = "csprojections"
)
future <- PoDDyHePoPopulationDF(
  survey, file = forecast_file, size = 400, y2pred = c(2030, 2040)
)

# 4. Use the selected formula. Future rows are excluded from model fitting.
projection <- PoDDyHePoProjection(
  future, m = 3, maxit = 3, seed = 2026,
  formulas = list(selection$`Suggested Model`)
)
projection$loggedEvents

# 5. Pool prevalences and plot observed and projected values.
pooled <- PoDDyHePoPool(projection, "smo", "sex", diagnostics = TRUE)
figure <- PoDDyHePoPlot(
  pooled, year = 2017, grplabels = c("Men", "Women"),
  title = "Current smoking: synthetic example", y_max = 60
)
print(figure)
```

`m = 3`, `maxit = 3` and `size = 400` keep the example quick. They are not
recommended production settings. Increase them as needed and check convergence
and sensitivity. An executable copy is installed at:

```r
system.file("examples", "workflow.R", package = "csprojections")
```

## Survey and population formats

Survey data need numeric integer `year`, numeric whole-year `age` and fully
observed `sex`. Use factors for binary and categorical indicators and numeric
columns for continuous variables. The default wide-file mapping is male = 1,
female = 2. Set `sex_map` when your codes differ. Formula methods are chosen
by `mice` from the variable types; numeric 0/1 data should be converted to a
factor if logistic imputation is intended.

Use either a semicolon-delimited Statistics Finland export (`file`) or a tidy
population data frame (`population`):

| year | age | sex | population |
| ---: | ---: | :-- | ---------: |
| 2030 | 25 | 1 | 10000 |
| 2030 | 25 | 2 | 10500 |
| 2030 | 26 | 1 | 10200 |
| 2030 | 26 | 2 | 10300 |

```r
future <- PoDDyHePoPopulationDF(
  survey, size = 10000, y2pred = c(2030, 2040),
  population = population_forecast
)
```

Supply every age between the survey minimum and maximum, every observed sex
code, and every requested year. Use one area/scenario at a time. Years are
matched by their values, independent of row or column order. Duplicate cells,
negative counts, missing cells, unsupported years, or age groups overlapping
the target ages cause an error.

The default largest-remainder allocation gives exactly `size` rows per year.
For random cell counts use `allocation = "multinomial", seed = 2026`. The
population inputs remain fixed in both cases: multinomial sampling does not
represent forecast-model uncertainty.

## Models and reproducibility

`PoDDyHePoModelSelection()` supports binomial, multinomial and Gaussian models.
Use `predictors` to exclude IDs and unwanted variables. By default it considers
two-way interactions, preserving the old candidate scope; set
`interactions = FALSE` for a smaller search. Mandatory terms go in `f.var`.

```r
selection <- PoDDyHePoModelSelection(
  historical, "smo", predictors = c("year", "sex", "age"),
  NsVar = c("year", "age"),
  knots = list(2007, c(35, 50)),
  b.knots = list(c(1997, 2017), c(25, 64)),
  f.var = c("year", "sex", "age", "year:sex")
)
```

Searches are performed within completed data sets. Every distinct winning
formula is then fitted to **every** completed data set and compared by mean
BIC. `Candidate BIC` records those comparisons and `Selection frequency`
records how often searches chose each formula. This is a transparent
heuristic, not a formal pooled model-selection criterion or a global optimum.
For research use, compare prespecified alternatives and check stability.

For projection, supply one formula for every incomplete variable. Existing
positional formulas still work when `printFlag` is explicitly supplied; a
named `formulas` list avoids R's positional argument matching:

```r
projection <- PoDDyHePoProjection(
  future, m = 20, maxit = 20, seed = 2026,
  formulas = list(smo ~ splines::ns(
    year, knots = 2007, Boundary.knots = c(1997, 2017)
  ) + sex + age)
)
```

Future rows receive imputations but do not train imputation models. This uses
the boundary attribute added by `PoDDyHePoPopulationDF()`. If you construct the
data yourself or lose its attributes, pass `observed_until` explicitly.
Auxiliary predictors are disabled so the formulas define the model exactly.
The optional seed is local: it does not reset the caller's random-number state.

## Interpreting pooled results

`PoDDyHePoPool()` handles numeric 0/1 outcomes directly, chooses the last level
for a binary factor, and returns every category of a multicategory factor.
Set `event = "Yes"` or another code to make the event explicit. Category
prevalences are calculated by equality to their values, never by subtracting
one from internal factor codes. Unused declared factor categories are retained.

`EST`, `CI_LOWER` and `CI_UPPER` are proportions. With `diagnostics = TRUE`, the
result also includes sample size, imputation count, Rubin total standard error,
MI-Wilson degrees of freedom, and within- and between-imputation variances.

Intervals use the MI-Wilson formula with a two-sided 95% confidence level by
default. The archived dependency used `qt(0.95, df)` for its default interval;
this version correctly uses `qt(0.975, df)`. Accordingly, intervals can be
wider even when point estimates agree. The point estimate is the mean of the
completed-data proportions. With one completed data set or no between-imputation
variation, the interval is the ordinary Wilson interval.

These intervals assume unweighted binomial observations. They do not account
for survey weights/clustering, model selection, uncertainty in the official
population forecast, or future changes outside the model. The synthetic sample
size changes the within-imputation term; do not interpret these summaries as
fully calibrated forecast prediction intervals. Check multiple sizes and
models, and use design-aware inference when required.

Reference: Lott A, Reiter JP (2020). *Wilson Confidence Intervals for Binomial
Proportions With Multiple Imputation for Missing Data*. The American
Statistician 74(2), 109–115. <https://doi.org/10.1080/00031305.2018.1473796>.
The original MIT-licensed MIWilson implementation is acknowledged in `inst/NOTICE`.

## Development checks

```r
install.packages(c("testthat", "roxygen2"))
testthat::test_local("csprojections")
roxygen2::roxygenise("csprojections")
```

```sh
R CMD build csprojections
R CMD check --no-manual csprojections_1.1.0.tar.gz
```

The developer ZIP includes a GitHub Actions configuration to run package checks
on Linux, Windows and macOS when added to a repository. Those hosted checks
have not been run as part of this local upgrade.

## Acknowledgement

The original R code was developed in the Projections of the burden of disease
and disability in Finland – health policy prospects (PoDDy-HePo) project,
funded by the Academy of Finland, grant 307907 (2017–2021). Existing authorship,
GPL-3 licensing and the original citation entry are retained; placeholder
`et al` entries have been removed from machine-readable author metadata.
