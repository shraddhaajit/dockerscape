test_that("1. Legal score construction and NA for isolated cases", {
  # Build simple 3-node graph: A connected to B (0.8) and C (0.4); D is isolated (not in graph)
  edges <- data.frame(
    from = c("A", "A"),
    to = c("B", "C"),
    weight = c(0.8, 0.4),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)

  df <- data.frame(
    case_id = c("A", "B", "C", "D"),
    court = "C1",
    filing_date = as.Date("2020-01-10"),
    duration = c(100, 120, 90, 80),
    event = c(1, 1, 0, 1),
    stringsAsFactors = FALSE
  )
  cl <- data.frame(court = "C1", date = as.Date("2020-01-01"), load = 10, stringsAsFactors = FALSE)

  fit <- fit_delay_model(df, g, cl)

  # Check legal score calculation:
  # Case A: mean(0.8, 0.4) = 0.6
  # Case B: mean(0.8) = 0.8
  # Case C: mean(0.4) = 0.4
  # Case D: NA
  md <- fit$model_data
  expect_equal(md$legal_score[md$case_id == "A"], 0.6)
  expect_equal(md$legal_score[md$case_id == "B"], 0.8)
  expect_equal(md$legal_score[md$case_id == "C"], 0.4)
  expect_false("D" %in% md$case_id)
  expect_equal(fit$exclusions["missing_legal_score"], c(missing_legal_score = 1))
})

test_that("2. Mandatory critical court-load alignment test", {
  # Setup exact prompt scenario:
  # Case A: court = C1, filing = 2020-02-15 -> month 2020-02-01 -> load 5
  # Case B: court = C1, filing = 2020-03-15 -> month 2020-03-01 -> load 8
  # Case C: court = C2, filing = 2020-02-15 -> month 2020-02-01 -> load 12 (must never receive C1 load)
  df <- data.frame(
    case_id = c("Case_A", "Case_B", "Case_C"),
    court = c("C1", "C1", "C2"),
    filing_date = as.Date(c("2020-02-15", "2020-03-15", "2020-02-15")),
    duration = c(100, 150, 200),
    event = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  cl <- data.frame(
    court = c("C1", "C1", "C2"),
    date = as.Date(c("2020-02-01", "2020-03-01", "2020-02-01")),
    load = c(5, 8, 12),
    stringsAsFactors = FALSE
  )

  # Complete graph among A, B, C with weight 0.5
  g <- igraph::make_full_graph(n = 3, directed = FALSE)
  igraph::V(g)$name <- c("Case_A", "Case_B", "Case_C")
  igraph::E(g)$weight <- 0.5

  fit <- fit_delay_model(df, g, cl)
  md <- fit$model_data

  expect_equal(md$load_at_filing[md$case_id == "Case_A"], 5)
  expect_equal(md$load_at_filing[md$case_id == "Case_B"], 8)
  expect_equal(md$load_at_filing[md$case_id == "Case_C"], 12)
})

test_that("3. Cox model fitting on deterministic adequate-size synthetic dataset", {
  set.seed(42)
  n <- 60
  case_ids <- paste0("C_", 1:n)
  courts <- sample(c("C1", "C2"), n, replace = TRUE)
  filing_dates <- sample(seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month"), n, replace = TRUE)
  durations <- round(runif(n, 30, 500))
  events <- rbinom(n, 1, 0.7)

  df <- data.frame(
    case_id = case_ids,
    court = courts,
    filing_date = filing_dates,
    duration = durations,
    event = events,
    stringsAsFactors = FALSE
  )

  # Graph with random edge weights
  el <- data.frame(
    from = sample(case_ids, 80, replace = TRUE),
    to = sample(case_ids, 80, replace = TRUE),
    weight = runif(80, 0.1, 0.9),
    stringsAsFactors = FALSE
  )
  el <- el[el$from != el$to, ]
  g <- igraph::graph_from_data_frame(el, directed = FALSE)

  # Court load table
  cl_dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month")
  cl <- expand.grid(court = c("C1", "C2"), date = cl_dates, stringsAsFactors = FALSE)
  cl$load <- round(runif(nrow(cl), 10, 50))

  fit <- fit_delay_model(df, g, cl)

  expect_s3_class(fit, "delay_model")
  expect_true(fit$fit_success)
  expect_true(fit$n_obs > 0)
  expect_true(fit$n_events > 0)
  expect_s3_class(fit$model, "coxph")
  expect_equal(nrow(fit$hazard_ratios), 2)
  expect_setequal(fit$hazard_ratios$term, c("legal_score", "load_at_filing"))
  expect_true(all(fit$hazard_ratios$hazard_ratio > 0))
})

test_that("4. All-censored observations handling", {
  df <- data.frame(
    case_id = c("Case_A", "Case_B"),
    court = "C1",
    filing_date = as.Date("2020-01-10"),
    duration = c(100, 150),
    event = c(0, 0),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(data.frame(from = "Case_A", to = "Case_B", weight = 0.5), directed = FALSE)
  cl <- data.frame(court = "C1", date = as.Date("2020-01-01"), load = 5, stringsAsFactors = FALSE)

  fit <- fit_delay_model(df, g, cl)
  expect_false(fit$fit_success)
  expect_equal(fit$n_events, 0)
  expect_null(fit$model)
})

test_that("5. Invalid inputs throw informative errors", {
  expect_error(fit_delay_model("not_df", NULL, NULL), "`cases_clean` must be a data.frame")
  expect_error(fit_delay_model(data.frame(a = 1), NULL, NULL), "Required column\\(s\\) missing")
})
