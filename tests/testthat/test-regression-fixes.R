# Regression tests for bug fixes and robustness improvements

test_that("clean_cases: single-row data frame correctly errors on non-existent filing column", {
  df1 <- data.frame(actual_filing = as.Date("2020-01-01"), stringsAsFactors = FALSE)
  expect_error(
    clean_cases(df1, filing = "missing_col", observation_end = "2024-12-31"),
    "Column 'missing_col' not found in `cases`"
  )
})

test_that("clean_cases: single-row data frame correctly errors on non-existent decision column", {
  df1 <- data.frame(filing_date = as.Date("2020-01-01"), stringsAsFactors = FALSE)
  expect_error(
    clean_cases(df1, decision = "missing_dec_col", observation_end = "2024-12-31"),
    "Column 'missing_dec_col' not found in `cases`"
  )
})

test_that("court_load: single-row data frame correctly errors on non-existent court column", {
  df1 <- data.frame(
    case_id = 1,
    actual_court = "Court_1",
    filing_date = as.Date("2020-01-01"),
    event = 1,
    duration = 10,
    stringsAsFactors = FALSE
  )
  expect_error(
    court_load(df1, court = "non_existent_court"),
    "Column 'non_existent_court' not found in `cases_clean`"
  )
})

test_that("fit_delay_model: single-row data frame correctly errors on non-existent court column", {
  df1 <- data.frame(
    case_id = "C1",
    filing_date = as.Date("2020-01-01"),
    duration = 100,
    event = 1,
    stringsAsFactors = FALSE
  )
  g <- igraph::make_empty_graph(n = 0, directed = FALSE)
  cl <- data.frame(court = "C1", date = as.Date("2020-01-01"), load = 5, stringsAsFactors = FALSE)
  expect_error(
    fit_delay_model(df1, g, cl, court = "missing_court_col"),
    "Column 'missing_court_col' not found in `cases_clean`"
  )
})

test_that("build_section_graph: 0 valid rows has 'name' vertex attribute defined", {
  empty_sec <- data.frame(case_id = character(0), act = character(0), section = character(0))
  g_empty <- build_section_graph(empty_sec)
  expect_true("name" %in% igraph::vertex_attr_names(g_empty))
  expect_equal(igraph::V(g_empty)$name, character(0))
})

test_that("fit_delay_model: composite key court load alignment accurately matches across courts and months", {
  df_multi <- data.frame(
    case_id = c("Case_1", "Case_2", "Case_3", "Case_4"),
    court = c("Court_A", "Court_A", "Court_B", "Court_B"),
    filing_date = as.Date(c("2020-01-15", "2020-02-15", "2020-01-20", "2020-02-10")),
    duration = c(50, 60, 70, 80),
    event = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  cl_multi <- data.frame(
    court = c("Court_A", "Court_A", "Court_B", "Court_B"),
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01")),
    load = c(10, 25, 40, 55),
    stringsAsFactors = FALSE
  )

  el <- data.frame(
    from = c("Case_1", "Case_2", "Case_3"),
    to = c("Case_2", "Case_3", "Case_4"),
    weight = c(0.5, 0.6, 0.7),
    stringsAsFactors = FALSE
  )
  nodes_df <- data.frame(name = paste0("Case_", 1:4), case_id = paste0("Case_", 1:4), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(el, vertices = nodes_df, directed = FALSE)

  fit <- fit_delay_model(df_multi, g, cl_multi)
  md <- fit$model_data
  expect_equal(nrow(md), 4)

  expect_equal(md$load_at_filing[md$case_id == "Case_1"], 10)
  expect_equal(md$load_at_filing[md$case_id == "Case_2"], 25)
  expect_equal(md$load_at_filing[md$case_id == "Case_3"], 40)
  expect_equal(md$load_at_filing[md$case_id == "Case_4"], 55)
})

test_that("Graph Invariant: build_section_graph has no self-loops or duplicate edges and has integer weights", {
  sec <- data.frame(
    case_id = c(1, 1, 1, 2, 2, 3),
    act = c("NI Act", "NI Act", "IPC", "NI Act", "IPC", "IPC"),
    section = c("138", "141", "420", "138", "420", "420"),
    stringsAsFactors = FALSE
  )
  g <- build_section_graph(sec)

  expect_false(igraph::any_loop(g))
  expect_false(igraph::any_multiple(g))
  expect_false(igraph::is_directed(g))
  expect_true(all(igraph::E(g)$weight >= 1))
  expect_true(all(igraph::E(g)$weight == floor(igraph::E(g)$weight)))
  expect_true("weight" %in% igraph::edge_attr_names(g))
})

test_that("Graph Invariant: diffuse_profiles row-normalization and sanitization of imperfect graph", {
  g_imperfect <- igraph::graph_from_data_frame(
    data.frame(
      from = c("A::1", "A::1", "A::1"),
      to = c("A::1", "A::2", "A::2"),
      weight = c(5, 2, 3),
      stringsAsFactors = FALSE
    ),
    vertices = data.frame(name = c("A::1", "A::2", "A::3"), act = "A", section = c("1", "2", "3"), stringsAsFactors = FALSE),
    directed = FALSE
  )
  sec_data <- data.frame(
    case_id = c("C1", "C2", "C3"),
    act = c("A", "A", "A"),
    section = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )

  diff_p <- diffuse_profiles(sec_data, g_imperfect, alpha = 0.5)

  r_sums <- Matrix::rowSums(diff_p$profiles)
  expect_equal(unname(as.numeric(r_sums)), rep(1.0, 3), tolerance = 1e-9)
  expect_equal(as.numeric(Matrix::rowSums(diff_p$profiles[3, , drop = FALSE])), 1.0)
})

test_that("Graph Invariant: build_case_graph has no self-loops, no duplicate edges, weights in (0, 1]", {
  syn_sec <- data.frame(
    case_id = c(1, 1, 2, 2, 3, 3, 4, 4),
    act = c("NI", "NI", "NI", "NI", "IPC", "IPC", "NI", "IPC"),
    section = c("138", "141", "138", "141", "420", "406", "138", "420"),
    stringsAsFactors = FALSE
  )
  sg <- build_section_graph(syn_sec)
  dp <- diffuse_profiles(syn_sec, sg, alpha = 0.7)
  cg <- build_case_graph(dp, k = 2)

  expect_false(igraph::any_loop(cg))
  expect_false(igraph::any_multiple(cg))
  expect_false(igraph::is_directed(cg))
  expect_true(all(igraph::E(cg)$weight > 0 & igraph::E(cg)$weight <= 1))
})

test_that("build_case_graph: rejects NA or empty case_ids with clear error", {
  mat <- Matrix::Matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, sparse = TRUE)
  prof_na <- list(profiles = mat, case_ids = c("C1", NA), section_ids = c("A::1", "A::2"))
  class(prof_na) <- "diffused_profiles"
  expect_error(build_case_graph(prof_na), "Missing or empty case IDs detected in `profiles`")

  prof_blank <- list(profiles = mat, case_ids = c("C1", "   "), section_ids = c("A::1", "A::2"))
  class(prof_blank) <- "diffused_profiles"
  expect_error(build_case_graph(prof_blank), "Missing or empty case IDs detected in `profiles`")
})

test_that("clean_cases: handles 0-row empty input data frame and all-censored cases", {
  empty_df <- data.frame(filing_date = as.Date(character(0)), decision_date = as.Date(character(0)), stringsAsFactors = FALSE)
  cleaned_empty <- clean_cases(empty_df, observation_end = "2024-12-31")
  expect_equal(nrow(cleaned_empty), 0)
  expect_true(all(c("duration", "event", "is_valid", "invalid_reason") %in% names(cleaned_empty)))

  df_cens <- data.frame(
    case_id = 1:2,
    filing_date = as.Date(c("2020-01-01", "2020-06-01")),
    decision_date = as.Date(c(NA, NA))
  )
  cleaned_cens <- clean_cases(df_cens, observation_end = "2020-12-31")
  expect_equal(cleaned_cens$event, c(0, 0))
  expect_equal(cleaned_cens$duration, c(365, 213))
  expect_true(all(cleaned_cens$is_valid))
})

test_that("court_load: handles all-censored cases, negative durations ignored, and single case", {
  df_cens <- data.frame(
    case_id = 1:2,
    court = c("C1", "C1"),
    filing_date = as.Date(c("2020-01-10", "2020-02-15")),
    decision_date = as.Date(c(NA, NA)),
    duration = c(356, 320),
    event = c(0, 0),
    is_valid = c(TRUE, TRUE)
  )
  cl <- court_load(df_cens, by = "month")
  expect_true(all(cl$load >= 0))
  expect_equal(cl$load[cl$date == as.Date("2020-03-01")], 2)

  df_neg <- data.frame(
    case_id = 1,
    court = "C1",
    filing_date = as.Date("2020-01-10"),
    duration = -5,
    event = 1,
    is_valid = TRUE
  )
  cl_neg <- court_load(df_neg)
  expect_equal(nrow(cl_neg), 0)
})

test_that("fit_delay_model: handles single case and < 3 cases gracefully without crashing", {
  df1 <- data.frame(
    case_id = "C1",
    court = "Court_1",
    filing_date = as.Date("2020-01-10"),
    duration = 50,
    event = 1,
    stringsAsFactors = FALSE
  )
  cl1 <- data.frame(court = "Court_1", date = as.Date("2020-01-01"), load = 10, stringsAsFactors = FALSE)
  el <- data.frame(from = "C1", to = "C2", weight = 0.5, stringsAsFactors = FALSE)
  nodes_df <- data.frame(name = c("C1", "C2"), case_id = c("C1", "C2"), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(el, vertices = nodes_df, directed = FALSE)

  fit1 <- fit_delay_model(df1, g, cl1)
  expect_false(fit1$fit_success)
  expect_match(fit1$fit_message, "Insufficient observations")
})

test_that("neighbourhood_survival: duplicate case_ids in cases_clean do not multiply neighbours", {
  df_dup <- data.frame(
    case_id = c("C1", "C2", "C2"),
    duration = c(100, 50, 50),
    event = c(1, 1, 1),
    is_valid = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  el <- data.frame(from = "C1", to = "C2", weight = 0.8, stringsAsFactors = FALSE)
  nodes_df <- data.frame(name = c("C1", "C2"), case_id = c("C1", "C2"), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(el, vertices = nodes_df, directed = FALSE)

  km <- neighbourhood_survival(g, df_dup, case_id = "C1")
  expect_equal(km$n_neighbours, 1)
  expect_equal(nrow(km$neighbours), 1)
})

test_that("neighbourhood_survival: all-censored neighbours handled cleanly", {
  df <- data.frame(
    case_id = c("C1", "C2", "C3"),
    duration = c(100, 200, 300),
    event = c(0, 0, 0),
    is_valid = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  el <- data.frame(from = c("C1", "C1"), to = c("C2", "C3"), weight = c(0.5, 0.7), stringsAsFactors = FALSE)
  nodes_df <- data.frame(name = c("C1", "C2", "C3"), case_id = c("C1", "C2", "C3"), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(el, vertices = nodes_df, directed = FALSE)

  km <- neighbourhood_survival(g, df, case_id = "C1")
  expect_true(is.na(km$median_survival))
  expect_equal(km$survival_table$surv, c(1.0))

  lat <- relative_lateness(km)
  expect_false(lat$estimable)
  expect_true(is.na(lat$relative_position))
})

test_that("Larger synthetic graph (1000 cases) graph invariants and consistency", {
  set.seed(42)
  n <- 1000
  cases_synth <- data.frame(
    case_id = paste0("Case_", 1:n),
    act = sample(c("IPC", "NI Act", "CrPC", "MV Act"), n * 2, replace = TRUE),
    section = sample(c("138", "420", "302", "154", "279", "304A"), n * 2, replace = TRUE),
    stringsAsFactors = FALSE
  )
  cases_synth$case_id <- rep(paste0("Case_", 1:n), each = 2)

  sg <- build_section_graph(cases_synth)
  expect_false(igraph::any_loop(sg))
  expect_false(igraph::any_multiple(sg))

  dp <- diffuse_profiles(cases_synth, sg, alpha = 0.7)
  expect_equal(length(dp$case_ids), n)
  expect_equal(as.numeric(Matrix::rowSums(dp$profiles)), rep(1.0, n), tolerance = 1e-9)

  cg <- build_case_graph(dp, k = 3)
  expect_false(igraph::any_loop(cg))
  expect_false(igraph::any_multiple(cg))
  expect_false(igraph::is_directed(cg))
  expect_equal(igraph::vcount(cg), n)
  expect_true(all(igraph::E(cg)$weight > 0 & igraph::E(cg)$weight <= 1))
})

test_that("Regression: clean_cases correctly masks duration to NA and flags invalid reasons for negative durations and post-cutoff dates", {
  df <- data.frame(
    case_id = c("C1", "C2", "C3", "C4"),
    filing_date = as.Date(c("2010-03-01", "2010-04-15", "2010-05-20", "2011-01-10")),
    decision_date = as.Date(c("2010-01-15", "2019-06-01", "2010-09-01", NA)), # C1 is before filing, C2 after cutoff
    stringsAsFactors = FALSE
  )
  cleaned <- clean_cases(df, observation_end = "2018-12-31")

  # C1: decision before filing
  expect_false(cleaned$is_valid[1])
  expect_equal(cleaned$invalid_reason[1], "Decision date before filing date")
  expect_true(is.na(cleaned$duration[1]))
  expect_true(is.na(cleaned$event[1]))

  # C2: decision after observation_end
  expect_false(cleaned$is_valid[2])
  expect_equal(cleaned$invalid_reason[2], "Decision date after observation_end")
  expect_true(is.na(cleaned$duration[2]))
  expect_true(is.na(cleaned$event[2]))

  # C3: valid disposed case
  expect_true(cleaned$is_valid[3])
  expect_true(is.na(cleaned$invalid_reason[3]))
  expect_equal(cleaned$event[3], 1)
  expect_equal(cleaned$duration[3], as.numeric(as.Date("2010-09-01") - as.Date("2010-05-20")))

  # C4: valid pending case
  expect_true(cleaned$is_valid[4])
  expect_true(is.na(cleaned$invalid_reason[4]))
  expect_equal(cleaned$event[4], 0)
  expect_equal(cleaned$duration[4], as.numeric(as.Date("2018-12-31") - as.Date("2011-01-10")))
})

test_that("Regression: project_validation_legal_score handles validation cases with unseen sections without erroring", {
  train_sec <- data.frame(
    case_id = c("T1", "T1", "T2", "T2"),
    act = c("IPC", "IPC", "IPC", "CrPC"),
    section = c("420", "406", "420", "154"),
    stringsAsFactors = FALSE
  )
  val_sec <- data.frame(
    case_id = c("V1", "V2", "V3"),
    act = c("IPC", "UnknownAct", "CrPC"),
    section = c("420", "999", "154"),
    stringsAsFactors = FALSE
  )

  proj <- project_validation_legal_score(
    val_sections = val_sec,
    train_sections = train_sec,
    alpha = 0.7,
    k = 2
  )

  expect_type(proj, "list")
  expect_true(all(c("validation_legal_scores", "unseen_section_cases", "val_cases_using_val_neighbours") %in% names(proj)))
  expect_equal(proj$val_cases_using_val_neighbours, 0L)

  scores <- proj$validation_legal_scores
  # V1 cites IPC 420 (known in train) -> valid projected score
  expect_true(!is.na(scores["V1"]))
  expect_true(scores["V1"] > 0)

  # V2 cites UnknownAct 999 (completely unseen) -> score is NA_real_
  expect_true(is.na(scores["V2"]))
  expect_true("V2" %in% proj$unseen_section_cases)

  # V3 cites CrPC 154 (known in train) -> valid projected score
  expect_true(!is.na(scores["V3"]))
})

test_that("Regression: validate_models handles chronological temporal splits on Date objects without quantile failure", {
  set.seed(99)
  n <- 30
  dates <- seq.Date(as.Date("2010-01-01"), as.Date("2010-12-01"), length.out = n)
  cases_df <- data.frame(
    case_id = paste0("C_", 1:n),
    court = sample(c("Court_1", "Court_2"), n, replace = TRUE),
    filing_date = dates,
    duration = sample(30:300, n, replace = TRUE),
    event = sample(c(0, 1), n, replace = TRUE),
    is_valid = rep(TRUE, n),
    stringsAsFactors = FALSE
  )

  cl <- court_load(cases_df, by = "month")
  sec_df <- data.frame(
    case_id = rep(paste0("C_", 1:n), each = 2),
    act = sample(c("IPC", "CrPC"), n * 2, replace = TRUE),
    section = sample(c("420", "154", "302"), n * 2, replace = TRUE),
    stringsAsFactors = FALSE
  )
  sg <- build_section_graph(sec_df)
  dp <- diffuse_profiles(sec_df, sg, alpha = 0.7)
  cg <- build_case_graph(dp, k = 3)

  fit <- fit_delay_model(cases_df, cg, cl)
  val <- validate_models(fit, sections = sec_df, temporal_split = 0.7)

  expect_s3_class(val, "model_validation")
  expect_true(val$n_train > 0)
  expect_true(val$n_val > 0)
  expect_equal(val$n_train + val$n_val, n)
  expect_true(is.numeric(val$validation_c_index) || is.na(val$validation_c_index))
})


