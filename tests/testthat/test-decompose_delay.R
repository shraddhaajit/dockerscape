test_that("1. Deterministic manual verification of decomposition and absolute shares", {
  # Create a mock delay_model with known Cox fit coefficients:
  # beta_legal = 0.5, beta_load = -0.1
  # Case A: legal_score = 0.6, load_at_filing = 5
  # Case B (Zero case): legal_score = 0, load_at_filing = 0
  mock_df <- data.frame(
    case_id = c("Case_A", "Case_B"),
    legal_score = c(0.6, 0.0),
    load_at_filing = c(5.0, 0.0),
    duration = c(100, 200),
    event = c(1, 1),
    stringsAsFactors = FALSE
  )

  # Fit a simple coxph model or mock object
  cox_fit <- suppressWarnings(survival::coxph(survival::Surv(duration, event) ~ legal_score + load_at_filing, data = mock_df))
  # Override coefficients manually for exact deterministic test
  cox_fit$coefficients["legal_score"] <- 0.5
  cox_fit$coefficients["load_at_filing"] <- -0.1

  mock_fit <- list(
    model = cox_fit,
    model_data = mock_df,
    coefficients = data.frame(term = c("legal_score", "load_at_filing"), estimate = c(0.5, -0.1)),
    fit_success = TRUE,
    fit_message = "Mock fit"
  )
  class(mock_fit) <- c("delay_model", "list")

  decomp <- decompose_delay(mock_fit)

  expect_s3_class(decomp, "delay_decomposition")
  expect_true(decomp$available)

  cd <- decomp$case_decomposition
  expect_equal(nrow(cd), 2)
  expect_setequal(
    names(cd),
    c("case_id", "legal_score", "load_at_filing", "legal_component",
      "load_component", "total_linear_predictor", "absolute_legal_share", "absolute_load_share")
  )

  # Case A verification:
  # legal_component = 0.5 * 0.6 = 0.30
  # load_component = -0.1 * 5.0 = -0.50
  # total_lp = -0.20
  # abs_legal_share = 0.30 / (0.30 + 0.50) = 0.375
  # abs_load_share = 0.50 / (0.30 + 0.50) = 0.625
  cA <- cd[cd$case_id == "Case_A", ]
  expect_equal(cA$legal_component, 0.30)
  expect_equal(cA$load_component, -0.50)
  expect_equal(cA$total_linear_predictor, -0.20)
  expect_equal(cA$absolute_legal_share, 0.375)
  expect_equal(cA$absolute_load_share, 0.625)
  expect_equal(cA$absolute_legal_share + cA$absolute_load_share, 1.0)

  # Case B (Zero case) verification:
  cB <- cd[cd$case_id == "Case_B", ]
  expect_equal(cB$legal_component, 0.0)
  expect_equal(cB$load_component, 0.0)
  expect_equal(cB$total_linear_predictor, 0.0)
  expect_true(is.na(cB$absolute_legal_share))
  expect_true(is.na(cB$absolute_load_share))
})

test_that("2. Non-converged model returns available = FALSE without fabrication", {
  non_conv_fit <- list(
    model = NULL,
    model_data = data.frame(case_id = "1", legal_score = 0.5, load_at_filing = 10, duration = 100, event = 1),
    fit_success = FALSE,
    fit_message = "Cox model fitting did not converge."
  )
  class(non_conv_fit) <- c("delay_model", "list")

  decomp <- decompose_delay(non_conv_fit)
  expect_s3_class(decomp, "delay_decomposition")
  expect_false(decomp$available)
  expect_equal(nrow(decomp$case_decomposition), 0)
})

test_that("3. Invalid inputs throw informative errors", {
  expect_error(decompose_delay("not_delay_model"), "`fit` must be a `delay_model` object")

  bad_model_data <- list(
    model = structure(list(coefficients = c(legal_score = 0.5, load_at_filing = 0.1)), class = "coxph"),
    model_data = data.frame(case_id = "1"), # missing required columns
    fit_success = TRUE
  )
  class(bad_model_data) <- c("delay_model", "list")
  expect_error(decompose_delay(bad_model_data), "Required column\\(s\\) missing")
})

test_that("4. Decomposition on adequate-size synthetic dataset model", {
  set.seed(99)
  n <- 50
  case_ids <- paste0("C_", 1:n)
  df <- data.frame(
    case_id = case_ids,
    court = sample(c("C1", "C2"), n, replace = TRUE),
    filing_date = sample(seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month"), n, replace = TRUE),
    duration = round(runif(n, 30, 400)),
    event = rbinom(n, 1, 0.8),
    stringsAsFactors = FALSE
  )

  el <- data.frame(
    from = sample(case_ids, 70, replace = TRUE),
    to = sample(case_ids, 70, replace = TRUE),
    weight = runif(70, 0.1, 0.9),
    stringsAsFactors = FALSE
  )
  el <- el[el$from != el$to, ]
  g <- igraph::graph_from_data_frame(el, directed = FALSE)

  cl <- expand.grid(court = c("C1", "C2"), date = seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month"), stringsAsFactors = FALSE)
  cl$load <- round(runif(nrow(cl), 10, 40))

  fit <- fit_delay_model(df, g, cl)
  decomp <- decompose_delay(fit)

  expect_s3_class(decomp, "delay_decomposition")
  expect_true(decomp$available)
  expect_equal(nrow(decomp$case_decomposition), fit$n_obs)
  expect_true(all(decomp$case_decomposition$absolute_legal_share + decomp$case_decomposition$absolute_load_share >= 0.9999 |
                  is.na(decomp$case_decomposition$absolute_legal_share)))
})
