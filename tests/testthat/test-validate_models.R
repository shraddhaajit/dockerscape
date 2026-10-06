test_that("1. Test A & B — Temporal ordering, chronological split, and disjoint case membership", {
  set.seed(42)
  n <- 40
  df <- data.frame(
    case_id = paste0("Case_", 1:n),
    filing_date = seq.Date(as.Date("2020-01-01"), by = "10 days", length.out = n),
    duration = round(runif(n, 30, 300)),
    event = rbinom(n, 1, 0.75),
    legal_score = runif(n, 0.2, 0.8),
    load_at_filing = round(runif(n, 10, 50)),
    stringsAsFactors = FALSE
  )

  cox_fit <- suppressWarnings(survival::coxph(survival::Surv(duration, event) ~ legal_score + load_at_filing, data = df))
  mock_fit <- list(
    model = cox_fit,
    model_data = df,
    hazard_ratios = data.frame(term = c("legal_score", "load_at_filing"), hazard_ratio = c(1.2, 1.01)),
    ph_test = NULL,
    fit_success = TRUE,
    fit_message = "Mock success"
  )
  class(mock_fit) <- c("delay_model", "list")

  val_res <- validate_models(mock_fit, temporal_split = 0.7)

  expect_s3_class(val_res, "model_validation")
  expect_true(val_res$available)

  # Check exact chronological boundary:
  # 70% of 40 = 28 training cases
  expect_equal(val_res$n_train, 28)
  expect_equal(val_res$n_val, 12)
  expect_equal(val_res$split_date, df$filing_date[28])
  expect_true(val_res$train_date_range[2] <= val_res$split_date)
  expect_true(val_res$val_date_range[1] > val_res$split_date)

  # Verify mutually exclusive case membership
  ord <- order(as.Date(df$filing_date), as.character(df$case_id))
  sorted_df <- df[ord, ]
  train_ids <- sorted_df$case_id[1:28]
  val_ids <- sorted_df$case_id[29:40]
  expect_equal(length(intersect(train_ids, val_ids)), 0)
})

test_that("2. Future-neighbour leakage audit test", {
  n <- 30
  df <- data.frame(
    case_id = paste0("C_", 1:n),
    filing_date = seq.Date(as.Date("2020-01-01"), by = "10 days", length.out = n),
    duration = round(runif(n, 30, 300)),
    event = rep(1, n),
    legal_score = runif(n, 0.2, 0.8),
    load_at_filing = round(runif(n, 10, 50)),
    stringsAsFactors = FALSE
  )

  # Connect training case C_1 (filing 2020-01-01) to validation case C_25 (filing 2020-08-28 > cutoff date)
  el <- data.frame(from = "C_1", to = "C_25", weight = 0.8, stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(el, directed = FALSE)

  cox_fit <- suppressWarnings(survival::coxph(survival::Surv(duration, event) ~ legal_score + load_at_filing, data = df))
  mock_fit <- list(
    model = cox_fit,
    model_data = df,
    case_graph = g,
    fit_success = TRUE,
    fit_message = "Mock success"
  )
  class(mock_fit) <- c("delay_model", "list")

  val_res <- validate_models(mock_fit, temporal_split = 0.7)

  expect_true(val_res$future_neighbour_leakage)
  expect_equal(val_res$n_training_cases_with_future_neighbours, 1)
  expect_equal(val_res$n_future_neighbour_edges, 1)
  expect_equal(val_res$max_future_neighbour_filing_date, df$filing_date[25])
})

test_that("3. Test D — Six-case non-converged model failure handling", {
  non_conv_fit <- list(
    model = NULL,
    model_data = data.frame(case_id = "1", filing_date = as.Date("2020-01-01"), duration = 10, event = 1, legal_score = 0.5, load_at_filing = 5),
    fit_success = FALSE,
    fit_message = "Cox model fitting did not converge: Ran out of iterations."
  )
  class(non_conv_fit) <- c("delay_model", "list")

  val_res <- validate_models(non_conv_fit)

  expect_s3_class(val_res, "model_validation")
  expect_false(val_res$available)
  expect_true(is.na(val_res$validation_c_index))
})

test_that("4. Test E & F — C-Index calculation and reproducibility on adequate dataset", {
  set.seed(123)
  n <- 60
  case_ids <- paste0("C_", 1:n)
  df <- data.frame(
    case_id = case_ids,
    court = sample(c("C1", "C2"), n, replace = TRUE),
    filing_date = sample(seq.Date(as.Date("2020-01-01"), as.Date("2020-08-01"), by = "month"), n, replace = TRUE),
    duration = round(runif(n, 30, 400)),
    event = rbinom(n, 1, 0.8),
    stringsAsFactors = FALSE
  )

  el <- data.frame(
    from = sample(case_ids, 80, replace = TRUE),
    to = sample(case_ids, 80, replace = TRUE),
    weight = runif(80, 0.1, 0.9),
    stringsAsFactors = FALSE
  )
  el <- el[el$from != el$to, ]
  g <- igraph::graph_from_data_frame(el, directed = FALSE)

  cl <- expand.grid(court = c("C1", "C2"), date = seq.Date(as.Date("2020-01-01"), as.Date("2020-08-01"), by = "month"), stringsAsFactors = FALSE)
  cl$load <- round(runif(nrow(cl), 10, 40))

  fit <- fit_delay_model(df, g, cl)
  val1 <- validate_models(fit, temporal_split = 0.7, seed = 123)
  val2 <- validate_models(fit, temporal_split = 0.7, seed = 123)

  expect_true(val1$available)
  expect_true(val1$training_c_index >= 0 && val1$training_c_index <= 1)
  expect_true(val1$validation_c_index >= 0 && val1$validation_c_index <= 1)
  expect_equal(val1, val2) # Reproducibility
})

test_that("5. Test G — Insufficient data handling", {
  tiny_df <- data.frame(
    case_id = c("1", "2", "3"),
    filing_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    duration = c(10, 20, 30),
    event = c(1, 1, 1),
    legal_score = c(0.5, 0.6, 0.7),
    load_at_filing = c(5, 10, 15)
  )
  cox_fit <- suppressWarnings(survival::coxph(survival::Surv(duration, event) ~ legal_score + load_at_filing, data = tiny_df))
  tiny_fit <- list(model = cox_fit, model_data = tiny_df, fit_success = TRUE)
  class(tiny_fit) <- c("delay_model", "list")

  val_res <- validate_models(tiny_fit)
  expect_false(val_res$available)
  expect_match(val_res$message, "Insufficient observations")
})

test_that("6. Test I & J — Censoring preservation and invalid input checks", {
  expect_error(validate_models("not_delay_model"), "`fit` must be a `delay_model` object")
  expect_error(validate_models(structure(list(), class = "delay_model"), temporal_split = 1.5), "`temporal_split` must be a single numeric value strictly between 0 and 1")

  set.seed(77)
  n <- 30
  df_cens <- data.frame(
    case_id = paste0("C_", 1:n),
    filing_date = seq.Date(as.Date("2020-01-01"), by = "10 days", length.out = n),
    duration = round(runif(n, 30, 300)),
    event = c(rep(0, 15), rep(1, 15)),
    legal_score = runif(n, 0.2, 0.8),
    load_at_filing = round(runif(n, 10, 50)),
    stringsAsFactors = FALSE
  )
  cox_fit <- suppressWarnings(survival::coxph(survival::Surv(duration, event) ~ legal_score + load_at_filing, data = df_cens))
  fit_cens <- list(model = cox_fit, model_data = df_cens, fit_success = TRUE)
  class(fit_cens) <- c("delay_model", "list")

  val_cens <- validate_models(fit_cens, temporal_split = 0.7)
  expect_true(val_cens$available)
  expect_equal(val_cens$n_train + val_cens$n_val, 30)
})
