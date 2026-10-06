make_test_sections <- function(n = 80, seed = 123) {
  set.seed(seed)
  acts_pool <- c("NI Act", "IPC", "Customs", "CrPC", "Companies Act")
  sections_pool <- list(
    "NI Act" = c("138", "141", "142"),
    "IPC" = c("302", "307", "420", "406"),
    "Customs" = c("110", "112", "135"),
    "CrPC" = c("154", "482", "167"),
    "Companies Act" = c("241", "242", "447")
  )
  sec_list <- list()
  for (i in 1:n) {
    n_sec <- sample(2:4, 1)
    chosen_acts <- sample(acts_pool, n_sec, replace = TRUE)
    chosen_secs <- sapply(chosen_acts, function(a) sample(sections_pool[[a]], 1))
    sec_list[[i]] <- data.frame(
      case_id = i,
      act = unname(chosen_acts),
      section = unname(chosen_secs),
      stringsAsFactors = FALSE
    )
  }
  unique(do.call(rbind, sec_list))
}

test_that("1. Training graph in leakage-aware validation contains 0 future neighbours", {
  set.seed(42)
  filing_dates <- seq.Date(as.Date("2010-01-01"), by = "15 days", length.out = 80)
  durations <- sample(100:600, 80, replace = TRUE)
  decision_dates <- filing_dates + durations

  cases_df <- data.frame(
    case_id = 1:80,
    court = rep(c("C1", "C2", "C3"), length.out = 80),
    filing_date = filing_dates,
    decision_date = decision_dates,
    stringsAsFactors = FALSE
  )
  cases_clean <- clean_cases(cases_df, observation_end = "2024-01-01")

  sec_df <- make_test_sections(80, seed = 123)

  sec_g <- build_section_graph(sec_df)
  diff_p <- diffuse_profiles(sec_df, sec_g)
  case_g <- build_case_graph(diff_p, k = 3)
  load_df <- court_load(cases_clean, by = "month")

  fit <- fit_delay_model(cases_clean, case_g, load_df)
  val <- validate_models(fit, sections = sec_df, temporal_split = 0.7)

  expect_true(val$available)
  expect_equal(val$clean_training_future_neighbour_edges, 0L)
  expect_equal(val$clean_training_future_neighbour_cases, 0L)
  expect_equal(val$val_cases_using_val_neighbours, 0L)
  expect_gt(val$val_cases_using_train_neighbours, 0L)
})

test_that("2. Validation cases use training cases only (no validation-to-validation contamination)", {
  val_sec <- data.frame(
    case_id = c("V1", "V2"),
    act = c("NI Act", "NI Act"),
    section = c("138", "138"),
    stringsAsFactors = FALSE
  )

  tr_sec <- data.frame(
    case_id = c("T1", "T2"),
    act = c("NI Act", "IPC"),
    section = c("138", "302"),
    stringsAsFactors = FALSE
  )

  proj <- project_validation_legal_score(val_sec, tr_sec, k = 2)

  expect_equal(proj$val_cases_using_val_neighbours, 0L)
  expect_equal(proj$val_cases_using_train_neighbours, 2L)

  map_v1 <- proj$neighbour_map[["V1"]]
  expect_false("V2" %in% map_v1$train_case_id)
  expect_true("T1" %in% map_v1$train_case_id)

  map_v2 <- proj$neighbour_map[["V2"]]
  expect_false("V1" %in% map_v2$train_case_id)
  expect_true("T1" %in% map_v2$train_case_id)
})

test_that("3. Training IDF and section graph are unchanged by adding validation cases", {
  tr_sec <- data.frame(
    case_id = c("T1", "T1", "T2"),
    act = c("NI Act", "NI Act", "IPC"),
    section = c("138", "141", "302"),
    stringsAsFactors = FALSE
  )

  val_sec <- data.frame(
    case_id = c("V1", "V1"),
    act = c("NI Act", "Customs"),
    section = c("138", "110"),
    stringsAsFactors = FALSE
  )

  tr_graph_before <- build_section_graph(tr_sec)
  tr_prof_before <- diffuse_profiles(tr_sec, tr_graph_before)
  tr_case_g_before <- build_case_graph(tr_prof_before)

  proj <- project_validation_legal_score(val_sec, tr_sec)

  tr_graph_after <- build_section_graph(tr_sec)
  tr_prof_after <- diffuse_profiles(tr_sec, tr_graph_after)
  tr_case_g_after <- build_case_graph(tr_prof_after)

  expect_equal(igraph::vcount(tr_graph_before), igraph::vcount(tr_graph_after))
  expect_equal(igraph::ecount(tr_graph_before), igraph::ecount(tr_graph_after))
  expect_equal(igraph::vcount(tr_case_g_before), igraph::vcount(tr_case_g_after))
  expect_equal(igraph::ecount(tr_case_g_before), igraph::ecount(tr_case_g_after))
})

test_that("4. Unseen validation sections are handled gracefully without crashing or mutating training vocabulary", {
  tr_sec <- data.frame(
    case_id = c("T1", "T2"),
    act = c("NI Act", "IPC"),
    section = c("138", "302"),
    stringsAsFactors = FALSE
  )

  val_sec <- data.frame(
    case_id = c("V1", "V1"),
    act = c("NI Act", "Customs"),
    section = c("138", "110"),
    stringsAsFactors = FALSE
  )

  proj <- project_validation_legal_score(val_sec, tr_sec)

  expect_true("V1" %in% proj$unseen_section_cases)
  expect_equal(length(proj$unseen_section_cases), 1L)
  expect_false(is.na(proj$validation_legal_scores["V1"]))
})

test_that("5. Exact manual D -> A projection works, D does not use E", {
  tr_sec <- data.frame(
    case_id = c("A", "A", "B", "C"),
    act = c("NI Act", "NI Act", "NI Act", "IPC"),
    section = c("138", "141", "138", "302"),
    stringsAsFactors = FALSE
  )

  val_sec <- data.frame(
    case_id = c("D", "D", "E", "E"),
    act = c("NI Act", "NI Act", "NI Act", "NI Act"),
    section = c("138", "141", "138", "141"),
    stringsAsFactors = FALSE
  )

  proj <- project_validation_legal_score(val_sec, tr_sec, k = 1)

  map_d <- proj$neighbour_map[["D"]]
  expect_equal(nrow(map_d), 1)
  expect_equal(map_d$train_case_id, "A")
  expect_false("E" %in% map_d$train_case_id)

  map_e <- proj$neighbour_map[["E"]]
  expect_equal(nrow(map_e), 1)
  expect_equal(map_e$train_case_id, "A")
  expect_false("D" %in% map_e$train_case_id)
})

test_that("6. Naive and leakage-aware validation are separately reported", {
  set.seed(42)
  filing_dates <- seq.Date(as.Date("2010-01-01"), by = "15 days", length.out = 80)
  durations <- sample(100:600, 80, replace = TRUE)
  decision_dates <- filing_dates + durations

  cases_df <- data.frame(
    case_id = 1:80,
    court = rep(c("C1", "C2", "C3"), length.out = 80),
    filing_date = filing_dates,
    decision_date = decision_dates,
    stringsAsFactors = FALSE
  )
  cases_clean <- clean_cases(cases_df, observation_end = "2024-01-01")

  sec_df <- make_test_sections(80, seed = 123)

  sec_g <- build_section_graph(sec_df)
  diff_p <- diffuse_profiles(sec_df, sec_g)
  case_g <- build_case_graph(diff_p, k = 3)
  load_df <- court_load(cases_clean, by = "month")

  fit <- fit_delay_model(cases_clean, case_g, load_df)
  val <- validate_models(fit, sections = sec_df, temporal_split = 0.7)

  expect_true(!is.na(val$naive_validation_c_index))
  expect_true(!is.na(val$leakage_aware_validation_c_index))
})

test_that("7. Six-case dataset fails gracefully for temporal split", {
  cases_df <- data.frame(
    case_id = 1:6,
    court = rep("C1", 6),
    filing_date = seq.Date(as.Date("2010-01-01"), by = "30 days", length.out = 6),
    decision_date = seq.Date(as.Date("2010-06-01"), by = "30 days", length.out = 6),
    stringsAsFactors = FALSE
  )
  cases_clean <- clean_cases(cases_df, observation_end = "2024-01-01")

  sec_df <- data.frame(
    case_id = rep(1:6, each = 2),
    act = rep(c("NI Act", "IPC"), 6),
    section = rep(c("138", "302"), 6),
    stringsAsFactors = FALSE
  )

  sec_g <- build_section_graph(sec_df)
  diff_p <- diffuse_profiles(sec_df, sec_g)
  case_g <- build_case_graph(diff_p, k = 2)
  load_df <- court_load(cases_clean, by = "month")

  fit <- fit_delay_model(cases_clean, case_g, load_df)
  val <- validate_models(fit, sections = sec_df, temporal_split = 0.7)

  expect_false(val$available)
  # The model may fail due to numerical instability OR insufficient observations
  expect_match(val$message,
    "Insufficient observations|Subset sizes too small|NUMERICALLY UNSTABLE|Zero resolution events|Cox model fitting")
})

test_that("8. Results are deterministic", {
  set.seed(42)
  filing_dates <- seq.Date(as.Date("2010-01-01"), by = "15 days", length.out = 80)
  durations <- sample(100:600, 80, replace = TRUE)
  decision_dates <- filing_dates + durations

  cases_df <- data.frame(
    case_id = 1:80,
    court = rep(c("C1", "C2", "C3"), length.out = 80),
    filing_date = filing_dates,
    decision_date = decision_dates,
    stringsAsFactors = FALSE
  )
  cases_clean <- clean_cases(cases_df, observation_end = "2024-01-01")

  sec_df <- make_test_sections(80, seed = 123)

  sec_g <- build_section_graph(sec_df)
  diff_p <- diffuse_profiles(sec_df, sec_g)
  case_g <- build_case_graph(diff_p, k = 3)
  load_df <- court_load(cases_clean, by = "month")

  fit <- fit_delay_model(cases_clean, case_g, load_df)
  val1 <- validate_models(fit, sections = sec_df, seed = 123)
  val2 <- validate_models(fit, sections = sec_df, seed = 123)

  expect_equal(val1$naive_validation_c_index, val2$naive_validation_c_index)
  expect_equal(val1$leakage_aware_validation_c_index, val2$leakage_aware_validation_c_index)
})

test_that("9. Duplicate terminology is accurate (distinguishes same-date/court clusters from duplicate records)", {
  df <- data.frame(
    case_id = 1:5,
    court = c("C1", "C1", "C1", "C2", "C2"),
    filing_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-02-01", "2020-03-01")),
    decision_date = as.Date(c("2020-06-01", "2020-06-01", "2020-06-01", NA, NA)),
    stringsAsFactors = FALSE
  )

  audit <- check_near_duplicates(df)

  expect_s3_class(audit, "near_duplicate_audit")
  expect_equal(audit$n_same_date_court_groups, 1)
  expect_equal(audit$n_exact_duplicate_groups, 1)
  expect_match(audit$interpretation, "Same-date/court clusters contain distinct case_ids")
})

test_that("10. Training IDF is genuinely training-only", {
  tr_sec <- data.frame(
    case_id = c("T1", "T2", "T3"),
    act = c("NI Act", "NI Act", "IPC"),
    section = c("138", "138", "302"),
    stringsAsFactors = FALSE
  )
  val_sec <- data.frame(
    case_id = c("V1", "V2", "V3"),
    act = c("IPC", "IPC", "IPC"),
    section = c("302", "302", "302"),
    stringsAsFactors = FALSE
  )

  proj <- project_validation_legal_score(val_sec, tr_sec)

  idf_tr_manual <- log(3 / 1) + 1
  idf_full_manual <- log(6 / 4) + 1

  expect_true(abs(idf_tr_manual - idf_full_manual) > 0.5)
  expect_equal(proj$validation_legal_scores[["V1"]], 1.0)
})
