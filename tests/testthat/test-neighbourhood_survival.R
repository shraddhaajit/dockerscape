test_that("1. Mandatory manual 3-neighbour weighted Kaplan-Meier calculation", {
  # Setup synthetic data matching prompt example:
  # Neighbour A (case 2): duration = 10, event = 1, weight = 1
  # Neighbour B (case 3): duration = 15, event = 0, weight = 1
  # Neighbour C (case 4): duration = 20, event = 1, weight = 2
  # Target Case (case 1)

  cases_clean <- data.frame(
    case_id = 1:4,
    filing_date = as.Date("2012-01-01"),
    decision_date = as.Date(c("2012-02-01", "2012-01-11", NA, "2012-01-21")),
    duration = c(31, 10, 15, 20),
    event = c(1, 1, 0, 1),
    is_valid = TRUE,
    invalid_reason = NA_character_,
    stringsAsFactors = FALSE
  )

  # Create a custom igraph case_graph connecting case 1 to cases 2, 3, 4
  nodes_df <- data.frame(name = as.character(1:4), case_id = as.character(1:4), stringsAsFactors = FALSE)
  edges_df <- data.frame(
    from = c("1", "1", "1"),
    to = c("2", "3", "4"),
    weight = c(1, 1, 2),
    stringsAsFactors = FALSE
  )
  cg <- igraph::graph_from_data_frame(edges_df, vertices = nodes_df, directed = FALSE)

  km_res <- neighbourhood_survival(cg, cases_clean, case_id = 1)

  expect_s3_class(km_res, "neighbourhood_survival")
  expect_equal(km_res$n_neighbours, 3)

  stab <- km_res$survival_table

  # Verify exact manual calculation:
  # t = 0:  Y = 4, d = 0, S = 1.00
  # t = 10: Y = 4, d = 1, S = 0.75
  # t = 20: Y = 2, d = 2, S = 0.00
  expect_equal(stab$time, c(0, 10, 20))
  expect_equal(stab$n_at_risk, c(4, 4, 2))
  expect_equal(stab$n_events, c(0, 1, 2))
  expect_equal(stab$surv, c(1.0, 0.75, 0.0))
  expect_equal(km_res$median_survival, 20)
})

test_that("2. Target case exclusion (no self-leakage)", {
  cases_clean <- get_synthetic_cases()
  cases_clean <- clean_cases(cases_clean, observation_end = "2014-12-31")
  sec_g <- build_section_graph(get_synthetic_sections())
  diff_p <- diffuse_profiles(get_synthetic_sections(), sec_g)
  cg <- build_case_graph(diff_p, k = 3)

  km_res <- neighbourhood_survival(cg, cases_clean, case_id = 1)

  # Target ID = "1" must not be in the neighbours list
  expect_false("1" %in% km_res$neighbours$case_id)
})

test_that("3. Pending vs Disposed target case status", {
  cases_clean <- get_synthetic_cases()
  cases_clean <- clean_cases(cases_clean, observation_end = "2014-12-31")
  sec_g <- build_section_graph(get_synthetic_sections())
  diff_p <- diffuse_profiles(get_synthetic_sections(), sec_g)
  cg <- build_case_graph(diff_p, k = 3)

  # Case 1 is disposed, Case 3 is pending
  km_c1 <- neighbourhood_survival(cg, cases_clean, case_id = 1)
  km_c3 <- neighbourhood_survival(cg, cases_clean, case_id = 3)

  expect_equal(km_c1$target_status, "disposed")
  expect_equal(km_c1$target_event, 1)

  expect_equal(km_c3$target_status, "pending")
  expect_equal(km_c3$target_event, 0)
})

test_that("4. Target case not found in cases_clean or case_graph", {
  cases_clean <- get_synthetic_cases()
  cases_clean <- clean_cases(cases_clean, observation_end = "2014-12-31")
  sec_g <- build_section_graph(get_synthetic_sections())
  diff_p <- diffuse_profiles(get_synthetic_sections(), sec_g)
  cg <- build_case_graph(diff_p, k = 3)

  expect_error(neighbourhood_survival(cg, cases_clean, case_id = 999))
})

test_that("5. Target case with no graph neighbours (isolated vertex)", {
  cases_clean <- get_synthetic_cases()
  cases_clean <- clean_cases(cases_clean, observation_end = "2014-12-31")

  # Graph with case 1 as isolated node
  nodes_df <- data.frame(name = as.character(1:3), case_id = as.character(1:3), stringsAsFactors = FALSE)
  edges_df <- data.frame(from = "2", to = "3", weight = 1, stringsAsFactors = FALSE)
  cg <- igraph::graph_from_data_frame(edges_df, vertices = nodes_df, directed = FALSE)

  km_res <- neighbourhood_survival(cg, cases_clean, case_id = 1)

  expect_equal(km_res$n_neighbours, 0)
  expect_true(is.na(km_res$median_survival))
  expect_equal(nrow(km_res$survival_table), 1)
})

test_that("6. All neighbours are censored (no events occur)", {
  cases_clean <- data.frame(
    case_id = 1:3,
    duration = c(30, 40, 50),
    event = c(0, 0, 0),
    is_valid = TRUE,
    stringsAsFactors = FALSE
  )
  nodes_df <- data.frame(name = as.character(1:3), case_id = as.character(1:3), stringsAsFactors = FALSE)
  edges_df <- data.frame(from = "1", to = c("2", "3"), weight = c(1, 1), stringsAsFactors = FALSE)
  cg <- igraph::graph_from_data_frame(edges_df, vertices = nodes_df, directed = FALSE)

  km_res <- neighbourhood_survival(cg, cases_clean, case_id = 1)

  expect_equal(km_res$n_neighbours, 2)
  expect_equal(km_res$survival_table$surv, 1.0)
  expect_true(is.na(km_res$median_survival))
})

test_that("7. Tied event times and unequal weights", {
  cases_clean <- data.frame(
    case_id = 1:4,
    duration = c(10, 20, 20, 30),
    event = c(1, 1, 1, 1),
    is_valid = TRUE,
    stringsAsFactors = FALSE
  )
  nodes_df <- data.frame(name = as.character(1:4), case_id = as.character(1:4), stringsAsFactors = FALSE)
  edges_df <- data.frame(
    from = "1",
    to = c("2", "3", "4"),
    weight = c(1, 2, 1),
    stringsAsFactors = FALSE
  )
  cg <- igraph::graph_from_data_frame(edges_df, vertices = nodes_df, directed = FALSE)

  km_res <- neighbourhood_survival(cg, cases_clean, case_id = 1)

  stab <- km_res$survival_table
  expect_equal(stab$time, c(0, 20, 30))
  expect_equal(stab$surv, c(1.0, 0.25, 0.0))
})

test_that("8. Reproducibility", {
  cases_clean <- get_synthetic_cases()
  cases_clean <- clean_cases(cases_clean, observation_end = "2014-12-31")
  sec_g <- build_section_graph(get_synthetic_sections())
  diff_p <- diffuse_profiles(get_synthetic_sections(), sec_g)
  cg <- build_case_graph(diff_p, k = 3)

  res1 <- neighbourhood_survival(cg, cases_clean, case_id = 1)
  res2 <- neighbourhood_survival(cg, cases_clean, case_id = 1)

  expect_equal(res1$survival_table, res2$survival_table)
  expect_equal(res1$neighbours, res2$neighbours)
})
