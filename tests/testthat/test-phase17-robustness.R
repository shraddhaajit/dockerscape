test_that("1. All cases pending handling", {
  cases_df <- data.frame(
    case_id = 1:10,
    court = rep("C1", 10),
    filing_date = seq.Date(as.Date("2020-01-01"), by = "10 days", length.out = 10),
    decision_date = as.Date(rep(NA, 10)),
    stringsAsFactors = FALSE
  )
  clean <- clean_cases(cases_df, observation_end = "2021-01-01")
  expect_equal(sum(clean$event == 1), 0)

  sec_df <- data.frame(case_id = 1:10, act = "NI Act", section = "138")
  sg <- build_section_graph(sec_df)
  dp <- diffuse_profiles(sec_df, sg)
  cg <- build_case_graph(dp, k = 2)
  ld <- court_load(clean, by = "month")

  fit <- fit_delay_model(clean, cg, ld)
  expect_false(fit$fit_success)
  expect_match(fit$fit_message, "Zero resolution events")
})

test_that("2. One observed event handling", {
  cases_df <- data.frame(
    case_id = 1:10,
    court = rep("C1", 10),
    filing_date = seq.Date(as.Date("2020-01-01"), by = "10 days", length.out = 10),
    decision_date = c(as.Date("2020-05-01"), rep(NA, 9)),
    stringsAsFactors = FALSE
  )
  clean <- clean_cases(cases_df, observation_end = "2021-01-01")
  expect_equal(sum(clean$event == 1), 1)

  sec_df <- data.frame(case_id = 1:10, act = "NI Act", section = "138")
  sg <- build_section_graph(sec_df)
  dp <- diffuse_profiles(sec_df, sg)
  cg <- build_case_graph(dp, k = 2)
  ld <- court_load(clean, by = "month")

  fit <- fit_delay_model(clean, cg, ld)
  expect_s3_class(fit, "delay_model")
})

test_that("3. One court only handling", {
  cases_df <- data.frame(
    case_id = 1:10,
    court = rep("C1", 10),
    filing_date = seq.Date(as.Date("2020-01-01"), by = "10 days", length.out = 10),
    decision_date = seq.Date(as.Date("2020-06-01"), by = "10 days", length.out = 10)
  )
  clean <- clean_cases(cases_df, observation_end = "2021-01-01")
  ld <- court_load(clean, by = "month")
  expect_equal(unique(ld$court), "C1")
})

test_that("4. Multiple courts handling", {
  cases_df <- data.frame(
    case_id = 1:12,
    court = rep(c("C1", "C2", "C3"), 4),
    filing_date = seq.Date(as.Date("2020-01-01"), by = "10 days", length.out = 12),
    decision_date = seq.Date(as.Date("2020-06-01"), by = "10 days", length.out = 12)
  )
  clean <- clean_cases(cases_df, observation_end = "2021-01-01")
  ld <- court_load(clean, by = "month")
  expect_equal(sort(unique(ld$court)), c("C1", "C2", "C3"))
})

test_that("5. Isolated legal sections handling", {
  sec_df <- data.frame(
    case_id = 1:3,
    act = c("NI Act", "IPC", "Customs"),
    section = c("138", "302", "110")
  )
  sg <- build_section_graph(sec_df)
  expect_equal(igraph::vcount(sg), 3)
  expect_equal(igraph::ecount(sg), 0)
})

test_that("6. Cases with no legal graph neighbours", {
  cases_df <- data.frame(
    case_id = 1:3,
    court = "C1",
    filing_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    decision_date = as.Date(c("2020-06-01", "2020-07-01", "2020-08-01"))
  )
  clean <- clean_cases(cases_df, observation_end = "2021-01-01")

  sec_df <- data.frame(
    case_id = 1:3,
    act = c("NI Act", "IPC", "Customs"),
    section = c("138", "302", "110")
  )
  sg <- build_section_graph(sec_df)
  dp <- diffuse_profiles(sec_df, sg)
  cg <- build_case_graph(dp, k = 2)

  expect_equal(igraph::ecount(cg), 0)

  km <- neighbourhood_survival(cg, clean, case_id = "1")
  expect_equal(km$n_neighbours, 0)
  expect_true(is.na(km$median_survival))
})

test_that("7, 8, 9. Missing court, filing date, decision date handling", {
  df_missing <- data.frame(
    case_id = 1:4,
    court = c("C1", NA, "C1", "C1"),
    filing_date = as.Date(c("2020-01-01", "2020-02-01", NA, "2020-04-01")),
    decision_date = as.Date(c("2020-06-01", "2020-07-01", "2020-08-01", NA))
  )
  clean <- clean_cases(df_missing, observation_end = "2021-01-01")

  # Case 3 has missing filing date -> is_valid = FALSE
  expect_false(clean$is_valid[3])

  sec_df <- data.frame(case_id = 1:4, act = "NI Act", section = "138")
  sg <- build_section_graph(sec_df)
  dp <- diffuse_profiles(sec_df, sg)
  cg <- build_case_graph(dp, k = 2)
  ld <- court_load(clean, by = "month")

  fit <- fit_delay_model(clean, cg, ld)
  # Missing court in case 2 excluded at model fitting step
  expect_gt(fit$exclusions[["missing_court"]], 0)
})

test_that("10. Validation case with unseen section", {
  tr_sec <- data.frame(case_id = c("T1", "T2"), act = c("NI Act", "IPC"), section = c("138", "302"))
  val_sec <- data.frame(case_id = "V1", act = "Motor Vehicles", section = "184")

  proj <- project_validation_legal_score(val_sec, tr_sec)

  expect_equal(proj$unseen_section_cases, "V1")
  expect_true(is.na(proj$validation_legal_scores["V1"]))
})

test_that("11. Very small training set handling in validation", {
  cases_df <- data.frame(
    case_id = 1:6,
    court = rep("C1", 6),
    filing_date = seq.Date(as.Date("2020-01-01"), by = "30 days", length.out = 6),
    decision_date = seq.Date(as.Date("2020-06-01"), by = "30 days", length.out = 6)
  )
  clean <- clean_cases(cases_df, observation_end = "2021-01-01")
  sec_df <- data.frame(case_id = rep(1:6, each = 2), act = "NI Act", section = c("138", "141"))
  sg <- build_section_graph(sec_df)
  dp <- diffuse_profiles(sec_df, sg)
  cg <- build_case_graph(dp, k = 2)
  ld <- court_load(clean, by = "month")
  fit <- fit_delay_model(clean, cg, ld)

  val <- validate_models(fit, sections = sec_df, temporal_split = 0.5)
  expect_false(val$available)
  expect_match(val$message,
    "Insufficient observations|Subset sizes too small|NUMERICALLY UNSTABLE|Zero resolution events|Cox model fitting")
})

test_that("12. No meaningful Cox variation handling", {
  model_data <- data.frame(
    case_id = 1:10,
    court = "C1",
    filing_date = as.Date("2020-01-01"),
    duration = rep(100, 10),
    event = rep(1, 10),
    legal_score = rep(1.0, 10),
    load_at_filing = rep(5.0, 10),
    stringsAsFactors = FALSE
  )
  fit_obj <- list(
    model = NULL,
    model_data = model_data,
    fit_success = TRUE,
    hazard_ratios = data.frame(),
    ph_test = NULL
  )
  class(fit_obj) <- c("delay_model", "list")

  val <- validate_models(fit_obj, temporal_split = 0.5)
  expect_s3_class(val, "model_validation")
})

test_that("13. Duplicate case IDs handling in case graph", {
  # Build case graph with duplicate case IDs in profiles
  prof_mat <- Matrix::sparseMatrix(i = c(1, 1, 2), j = c(1, 2, 1), x = 0.5, dims = c(3, 2))
  bad_profiles <- list(
    profiles = prof_mat,
    case_ids = c("1", "1", "2"), # Duplicate case_id "1"
    section_ids = c("NI Act::138", "IPC::302")
  )
  class(bad_profiles) <- "diffused_profiles"

  expect_error(build_case_graph(bad_profiles), "Duplicate case IDs")
})

test_that("14. Duplicate same-date/court records audit", {
  df_same_date <- data.frame(
    case_id = 1:3,
    court = "C1",
    filing_date = as.Date("2020-01-01"),
    decision_date = as.Date("2020-06-01")
  )
  audit <- check_near_duplicates(df_same_date)
  expect_equal(audit$n_same_date_court_groups, 1)
  expect_equal(audit$n_exact_duplicate_groups, 1)
})

test_that("15. Case with zero legal similarity to every training case", {
  tr_sec <- data.frame(case_id = c("T1", "T2"), act = "NI Act", section = "138")
  val_sec <- data.frame(case_id = "V1", act = "IPC", section = "302")

  proj <- project_validation_legal_score(val_sec, tr_sec)

  expect_true(is.na(proj$validation_legal_scores["V1"]))
  expect_equal(proj$val_cases_using_train_neighbours, 0L)
  expect_equal(proj$val_cases_using_val_neighbours, 0L)
})
