test_that("1. Mandatory manual single-court C1 monthly load calculation", {
  # Setup exact manual example from prompt:
  # Case A: filing = 2020-01-10, decision = 2020-03-15
  # Case B: filing = 2020-02-05, pending through 2020-04-30
  # Case C: filing = 2020-03-20, decision = 2020-03-25

  df <- data.frame(
    case_id = 1:3,
    court = "C1",
    filing_date = as.Date(c("2020-01-10", "2020-02-05", "2020-03-20")),
    decision_date = as.Date(c("2020-03-15", NA, "2020-03-25"))
  )
  cleaned <- clean_cases(df, observation_end = "2020-04-30")

  load_c1 <- court_load(cleaned, by = "month")

  expect_s3_class(load_c1, "court_load")
  expect_equal(unique(load_c1$court), "C1")
  expect_equal(load_c1$date, as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")))

  # Expected loads: 2020-01-01 -> 0, 2020-02-01 -> 1, 2020-03-01 -> 2, 2020-04-01 -> 1
  expect_equal(load_c1$load, c(0, 1, 2, 1))
})

test_that("2. Multiple courts calculated independently (no cross-leakage)", {
  df <- data.frame(
    case_id = 1:4,
    court = c("C1", "C1", "C2", "C2"),
    filing_date = as.Date(c("2012-01-10", "2012-03-05", "2012-01-10", "2012-05-15")),
    decision_date = as.Date(c("2012-11-02", NA, "2012-02-14", NA))
  )
  cleaned <- clean_cases(df, observation_end = "2012-12-31")

  load_df <- court_load(cleaned, by = "month")

  expect_setequal(unique(load_df$court), c("C1", "C2"))
  c1_loads <- load_df$load[load_df$court == "C1"]
  c2_loads <- load_df$load[load_df$court == "C2"]
  expect_equal(length(c1_loads), length(c2_loads))
})

test_that("3. Pending case remains open through observation window", {
  df <- data.frame(
    case_id = 1,
    court = "C1",
    filing_date = as.Date("2012-01-10"),
    decision_date = as.Date(NA)
  )
  cleaned <- clean_cases(df, observation_end = "2012-04-30")

  load_df <- court_load(cleaned, by = "month")

  # Dates: 2012-01-01 (0), 2012-02-01 (1), 2012-03-01 (1), 2012-04-01 (1)
  expect_equal(load_df$load, c(0, 1, 1, 1))
})

test_that("4. Disposed case exactly on monthly time point is no longer open", {
  # Case disposed on 2012-03-01 (exact monthly time point)
  df <- data.frame(
    case_id = 1,
    court = "C1",
    filing_date = as.Date("2012-01-10"),
    decision_date = as.Date("2012-03-01")
  )
  cleaned <- clean_cases(df, observation_end = "2012-04-30")

  load_df <- court_load(cleaned, by = "month")

  # At 2012-03-01: decision_date > t is FALSE (since 2012-03-01 > 2012-03-01 is FALSE), so NOT open at March 1!
  expect_equal(load_df$load[load_df$date == as.Date("2012-03-01")], 0)
})

test_that("5. Daily mode by = 'day'", {
  df <- data.frame(
    case_id = 1:2,
    court = "C1",
    filing_date = as.Date(c("2020-01-01", "2020-01-03")),
    decision_date = as.Date(c("2020-01-04", NA))
  )
  cleaned <- clean_cases(df, observation_end = "2020-01-05")

  load_daily <- court_load(cleaned, by = "day")

  expect_equal(nrow(load_daily), 5)
  # Jan 1: case 1 open (1)
  # Jan 2: case 1 open (1)
  # Jan 3: case 1 open, case 2 open (2)
  # Jan 4: case 1 disposed on Jan 4 -> not open on Jan 4 (1)
  # Jan 5: case 2 open (1)
  expect_equal(load_daily$load, c(1, 1, 2, 1, 1))
})

test_that("6. Invalid case rows are excluded", {
  df <- data.frame(
    case_id = 1:2,
    court = c("C1", "C1"),
    filing_date = as.Date(c("2012-01-10", "2012-05-10")),
    decision_date = as.Date(c("2012-11-02", "2012-01-01")) # case 2 is invalid (decision before filing)
  )
  cleaned <- clean_cases(df, observation_end = "2012-12-31")

  load_df <- court_load(cleaned, by = "month")

  # Only valid case 1 contributes
  expect_true(all(load_df$load >= 0))
  expect_equal(max(load_df$load), 1)
})

test_that("7. Empty input handling", {
  empty_cases <- data.frame(
    case_id = numeric(0),
    court = character(0),
    filing_date = as.Date(character(0)),
    decision_date = as.Date(character(0)),
    duration = numeric(0),
    event = numeric(0),
    is_valid = logical(0)
  )

  load_empty <- court_load(empty_cases, by = "month")

  expect_s3_class(load_empty, "court_load")
  expect_equal(nrow(load_empty), 0)
})

test_that("8. Non-negative loads and deterministic ordering", {
  syn_cases <- get_synthetic_cases()
  cleaned <- clean_cases(syn_cases, observation_end = "2014-12-31")

  l1 <- court_load(cleaned, by = "month")
  l2 <- court_load(cleaned, by = "month")

  expect_true(all(l1$load >= 0))
  expect_equal(l1, l2)
  expect_true(!is.unsorted(l1$court))
})
