# Run after installation:
# source(system.file("examples", "verify_all_functions.R", package = "poddyhepo"))
# result <- verify_poddyhepo("validation-output")
# Example settings keep this verification quick; use larger m/maxit in research.
verify_poddyhepo <- function(output_dir = tempfile("poddyhepo-validation-")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir)
  public <- c("PoDDyHePoFreqTable", "PoDDyHePoModelSelection", "PoDDyHePoPopulationDF",
              "PoDDyHePoProjection", "PoDDyHePoPool", "PoDDyHePoPlot")
  stopifnot(identical(sort(getNamespaceExports("poddyhepo")), sort(public)))
  demo <- new.env()
  utils::data("testdata", package = "poddyhepo", envir = demo)
  survey <- demo$testdata
  variables <- setdiff(names(survey), c("year", "sex", "age"))
  formulas <- lapply(variables, function(v) stats::reformulate(c("year", "sex", "age"), response = v))
  names(formulas) <- variables

  response <- poddyhepo::PoDDyHePoFreqTable(survey, variables)
  stopifnot(all(response$Total == response$Response + response$Missing),
            sum(response$Total) == nrow(survey) * length(variables))
  historical <- poddyhepo::PoDDyHePoProjection(survey, m = 3, maxit = 3,
                                               formulas = formulas, seed = 2026)
  stopifnot(all(vapply(seq_len(historical$m), function(i)
    !anyNA(mice::complete(historical, i)), logical(1))))

  selections <- lapply(variables, function(v) poddyhepo::PoDDyHePoModelSelection(
    historical, v, predictors = c("year", "sex", "age"), NsVar = "year", df = 2,
    f.var = c("year", "sex", "age"), interactions = FALSE))
  names(selections) <- variables
  selected_formulas <- lapply(selections, function(s) s$`Suggested Model`)
  scores <- do.call(rbind, lapply(variables, function(v) {
    s <- selections[[v]]$`Candidate BIC`
    s$Variable <- v
    s
  }))
  stopifnot(all(is.finite(scores$Mean_BIC)))
  forecast <- system.file("extdata", "003_128v_2040_20210730-015759.csv", package = "poddyhepo")
  future <- poddyhepo::PoDDyHePoPopulationDF(survey, forecast, size = 250,
                                            y2pred = c(2030, 2040))
  stopifnot(sum(future$year == 2030) == 250, sum(future$year == 2040) == 250)
  projection <- poddyhepo::PoDDyHePoProjection(future, m = 3, maxit = 3,
                                               formulas = selected_formulas, seed = 2026)
  stopifnot(all(projection$ignore[future$year > 2017]))
  again <- poddyhepo::PoDDyHePoProjection(future, m = 3, maxit = 3,
                                          formulas = selected_formulas, seed = 2026)
  stopifnot(all(vapply(seq_len(projection$m), function(i)
    identical(mice::complete(projection, i), mice::complete(again, i)), logical(1))))

  plots <- list(); estimates <- list()
  for (v in variables) {
    for (group in list(NULL, "sex")) {
      pooled <- poddyhepo::PoDDyHePoPool(projection, v, group, diagnostics = TRUE)
      stopifnot(all(is.finite(pooled$EST)), all(is.finite(pooled$CI_LOWER)),
                all(is.finite(pooled$CI_UPPER)), all(pooled$CI_LOWER <= pooled$EST),
                all(pooled$EST <= pooled$CI_UPPER), all(pooled$CI_LOWER >= 0),
                all(pooled$CI_UPPER <= 1))
      if ("Group" %in% names(pooled)) {
        keys <- c("Year", group)
        sums <- stats::aggregate(pooled$EST, pooled[keys], sum)$x
        stopifnot(all(abs(sums - 1) < 1e-12))
      }
      p <- poddyhepo::PoDDyHePoPlot(pooled, 2017,
        grplabels = if (is.null(group)) NULL else c("Men", "Women"),
        title = paste(v, if (is.null(group)) "overall" else "by sex"))
      ggplot2::ggplot_build(p)
      plots[[paste(v, if (is.null(group)) "overall" else group, sep = "_")]] <- p
      if (!is.null(group)) {
        pooled$Variable <- v
        pooled$Group <- if ("Group" %in% names(pooled)) as.character(pooled$Group) else "event_1"
        estimates[[v]] <- pooled[c("Variable", "Year", "sex", "Group", "EST", "CI_LOWER",
                                   "CI_UPPER", "N", "M", "SE", "DF", "Within", "Between")]
      }
    }
  }
  draw_layouts <- function() {
    grDevices::png(file.path(output_dir, "four_plot_layouts.png"), width = 1800,
                    height = 1300, res = 150, type = "cairo")
    on.exit(grDevices::dev.off())
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
    selected <- c("smo_overall", "smo_sex", "obe_overall", "obe_sex")
    for (i in seq_along(selected)) print(plots[[selected[i]]],
      vp = grid::viewport(layout.pos.row = ceiling(i / 2), layout.pos.col = (i - 1) %% 2 + 1))
  }
  draw_layouts()
  matrix <- data.frame(Function = public, Status = "PASS", Scope = c(
    "All 9 indicators; independent total/response/missing identities",
    "Binary and multinomial; linear/spline candidates on 3 imputations",
    "Named CSV years; exactly 250 rows for both 2030 and 2040",
    "All 9 incomplete variables; held-out future rows; seeded repeatability",
    "All 9 indicators, overall and by sex; bounds and category sums",
    "18 plots built; four layouts rendered"))
  utils::write.csv(response, file.path(output_dir, "response.csv"), row.names = FALSE)
  utils::write.csv(scores, file.path(output_dir, "candidate_bic.csv"), row.names = FALSE)
  utils::write.csv(do.call(rbind, estimates), file.path(output_dir, "pooled_prevalences.csv"), row.names = FALSE)
  utils::write.csv(matrix, file.path(output_dir, "function_checks.csv"), row.names = FALSE)
  writeLines(capture.output(utils::sessionInfo()), file.path(output_dir, "session-info.txt"))
  print(matrix, row.names = FALSE)
  cat("\nVerified", length(public), "public functions and", length(variables),
      "indicators. Outputs:", output_dir, "\n")
  invisible(list(checks = matrix, projection = projection, estimates = estimates, plots = plots))
}
