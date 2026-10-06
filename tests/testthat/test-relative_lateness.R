test_that("1. CASE A — Informative event experience within supported KM range", {
  stab <- data.frame(
    time = c(0, 10, 20),
    n_at_risk = c(4, 4, 2),
    n_events = c(0, 1, 2),
    surv = c(1.00, 0.75, 0.00)
  )
  neigh_df <- data.frame(
    case_id = c("2", "3", "4"),
    weight = c(1, 1, 2),
    duration = c(10, 15, 20),
    event = c(1, 0, 1),
    stringsAsFactors = FALSE
  )
  mock_km <- list(
    target_case_id = "1",
    target_status = "pending",
    target_duration = 10,
    neighbours = neigh_df,
    n_neighbours = 3,
    survival_table = stab,
    median_survival = 20
  )
  class(mock_km) <- "neighbourhood_survival"

  res <- relative_lateness(mock_km)

  expect_true(res$estimable)
  expect_equal(res$survival_at_duration, 0.75)
  expect_equal(res$relative_position, 0.25)
})

test_that("2. CASE B — All neighbours censored before target age", {
  neigh_df <- data.frame(
    case_id = "6",
    weight = 1,
    duration = 350,
    event = 0,
    stringsAsFactors = FALSE
  )
  stab <- data.frame(time = 0, n_at_risk = 1, n_events = 0, surv = 1.0)
  mock_km <- list(
    target_case_id = "3",
    target_status = "pending",
    target_duration = 924,
    neighbours = neigh_df,
    n_neighbours = 1,
    survival_table = stab,
    median_survival = NA_real_
  )
  class(mock_km) <- "neighbourhood_survival"

  res <- relative_lateness(mock_km)

  expect_false(res$estimable)
  expect_true(is.na(res$relative_position))
  expect_true(is.na(res$survival_at_duration))
  expect_false(grepl("0th percentile", res$interpretation))
  expect_match(res$interpretation, "all 1 neighbouring cases are censored")
})

test_that("3. CASE C — Events observed, target is beyond final event while follow-up continues", {
  # Event at 100, additional neighbour censored at 500, target duration 400
  neigh_df <- data.frame(
    case_id = c("2", "3"),
    weight = c(1, 1),
    duration = c(100, 500),
    event = c(1, 0),
    stringsAsFactors = FALSE
  )
  stab <- data.frame(
    time = c(0, 100),
    n_at_risk = c(2, 2),
    n_events = c(0, 1),
    surv = c(1.0, 0.5)
  )
  mock_km <- list(
    target_case_id = "1",
    target_status = "pending",
    target_duration = 400,
    neighbours = neigh_df,
    n_neighbours = 2,
    survival_table = stab,
    median_survival = 100
  )
  class(mock_km) <- "neighbourhood_survival"

  res <- relative_lateness(mock_km)

  expect_true(res$estimable)
  expect_equal(res$survival_at_duration, 0.5)
  expect_equal(res$relative_position, 0.5)
})

test_that("4. CASE D — Survival has reached zero", {
  neigh_df <- data.frame(
    case_id = c("2", "3"),
    weight = c(1, 1),
    duration = c(10, 20),
    event = c(1, 1),
    stringsAsFactors = FALSE
  )
  stab <- data.frame(
    time = c(0, 10, 20),
    n_at_risk = c(2, 2, 1),
    n_events = c(0, 1, 1),
    surv = c(1.0, 0.5, 0.0)
  )
  mock_km <- list(
    target_case_id = "1",
    target_status = "pending",
    target_duration = 25,
    neighbours = neigh_df,
    n_neighbours = 2,
    survival_table = stab,
    median_survival = 10
  )
  class(mock_km) <- "neighbourhood_survival"

  res <- relative_lateness(mock_km)

  expect_true(res$estimable)
  expect_equal(res$survival_at_duration, 0.0)
  expect_equal(res$relative_position, 1.0)
})

test_that("5. Mandatory manual step-function test table values", {
  neigh_df <- data.frame(
    case_id = c("2", "3", "4"),
    weight = c(1, 1, 2),
    duration = c(10, 15, 20),
    event = c(1, 0, 1),
    stringsAsFactors = FALSE
  )
  stab <- data.frame(
    time = c(0, 10, 20),
    n_at_risk = c(4, 4, 2),
    n_events = c(0, 1, 2),
    surv = c(1.00, 0.75, 0.00)
  )

  make_mock_km <- function(target_duration, target_status = "pending") {
    obj <- list(
      target_case_id = "1",
      target_status = target_status,
      target_duration = target_duration,
      target_event = if (target_status == "disposed") 1 else 0,
      neighbours = neigh_df,
      n_neighbours = 3,
      survival_table = stab,
      median_survival = 20
    )
    class(obj) <- "neighbourhood_survival"
    obj
  }

  res5 <- relative_lateness(make_mock_km(5))
  expect_true(res5$estimable)
  expect_equal(res5$survival_at_duration, 1.00)
  expect_equal(res5$relative_position, 0.00)

  res10 <- relative_lateness(make_mock_km(10))
  expect_true(res10$estimable)
  expect_equal(res10$survival_at_duration, 0.75)
  expect_equal(res10$relative_position, 0.25)

  res15 <- relative_lateness(make_mock_km(15))
  expect_true(res15$estimable)
  expect_equal(res15$survival_at_duration, 0.75)
  expect_equal(res15$relative_position, 0.25)

  res20 <- relative_lateness(make_mock_km(20))
  expect_true(res20$estimable)
  expect_equal(res20$survival_at_duration, 0.00)
  expect_equal(res20$relative_position, 1.00)

  res30 <- relative_lateness(make_mock_km(30))
  expect_true(res30$estimable)
  expect_equal(res30$survival_at_duration, 0.00)
  expect_equal(res30$relative_position, 1.00)
})

test_that("6. Case 3 from README synthetic dataset evaluates estimable = FALSE and NA relative position", {
  syn_cases <- get_synthetic_cases()
  clean_syn <- clean_cases(syn_cases, observation_end = "2014-12-31")
  sec_g <- build_section_graph(get_synthetic_sections())
  diff_p <- diffuse_profiles(get_synthetic_sections(), sec_g)
  case_g <- build_case_graph(diff_p, k = 3)

  km_c3 <- neighbourhood_survival(case_g, clean_syn, case_id = 3)
  res_c3 <- relative_lateness(km_c3)

  expect_false(res_c3$estimable)
  expect_true(is.na(res_c3$relative_position))
  expect_true(is.na(res_c3$survival_at_duration))
  expect_false(grepl("0th percentile", res_c3$interpretation))
})

test_that("7. Invalid inputs and 0-neighbour handling", {
  expect_error(relative_lateness(NULL))
  expect_error(relative_lateness("invalid"))

  mock_empty <- list(
    target_case_id = "1",
    target_status = "pending",
    target_duration = 10,
    neighbours = data.frame(),
    n_neighbours = 0,
    survival_table = data.frame(),
    median_survival = NA_real_
  )
  class(mock_empty) <- "neighbourhood_survival"

  res_empty <- relative_lateness(mock_empty)
  expect_false(res_empty$estimable)
  expect_true(is.na(res_empty$relative_position))
})
