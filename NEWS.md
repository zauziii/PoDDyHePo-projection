# csprojections 1.1.0

Development upgrade reconstructed from the supplied 1.0.0 text snapshot.
Not an upstream or CRAN release.

* Export `PoDDyHePoProjection`, which was absent from the original namespace.
* Replace the frequency table's undefined `sukup` with the requested grouping
  column, return a reusable data frame, and allow multiple indicators.
* Repair linear model selection and fixed-term handling. Support binomial,
  multinomial and Gaussian outcomes, multiple spline variables, multiple
  interior knots, fixed spline bases, and explicit predictor selection.
* Compare every distinct candidate formula on every imputation before ranking
  mean BIC. Report candidate scores and search-selection frequencies.
* Match population forecasts by named year/sex columns or tidy keys. Validate
  coverage and reject duplicate cells and ambiguous area exports. Retain real
  age values and factor types. Allocate exactly `size` rows per future year.
* Validate projection formulas and controls, support a named formula list,
  expose optional methods and extra mice controls, disable auxiliary predictors,
  and exclude future rows from training when the observed boundary is known.
* Scope explicit seeds without resetting the caller's RNG state.
* Repair numeric binary and multicategory pooling. Permit explicit event
  selection, retain unused categories and expose optional variance diagnostics.
* Replace archived MIWilson with an attributed local implementation. Correct
  the two-sided critical quantile from `qt(conf.level)` to
  `qt((1 + conf.level) / 2)`. Handle one data set, constant proportions and
  infinite variance inflation without undefined values.
* Consolidate plotting, fix titles and grouping, support more categories, and
  bridge each series from its last observation into its projected segment.
  Use `linewidth` and viewing limits that do not discard intervals.
* Remove unnecessary attached dependencies, regenerate help and namespace,
  provide a complete example, and add tests and a hosted-check configuration.
* Supply a newly generated synthetic `testdata`; the original binary data was
  absent from the supplied snapshot. No published results are reconstructed.

## Migration notes

The six function names and their original leading arguments are retained.
Frequency summaries now return a data frame rather than printing a qwraps2
table. Pooling defaults to a true two-sided 95% interval, so old intervals will
change. Explicit event coding is recommended. Population allocation, model
choice and projection formulas can also change results after the corrections.
Supplying named lists via `formulas` is preferred; every incomplete variable
must have exactly one formula. Numeric binary columns remain numeric unless
the user explicitly converts them to factors for logistic imputation.

The package now requires R >= 4.1.0 and ggplot2 >= 3.4.0. This is a source
development upgrade, not a claim of cross-platform or statistical validation
for all projection designs.
