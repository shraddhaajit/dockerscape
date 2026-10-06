test_that("1. Normal disposed case", {
  df <- data.frame(
    filing_date = as.Date("2012-01-10"),
    decision_date = as.Date("2012-11-02")
  )
  res <- clean_cases(df, observation_end = "2014-12-31")

  expect_true(res$is_valid)
  expect_true(is.na(res$invalid_reason))
  expect_equal(res$event, 1)
  expect_equal(res$duration, as.numeric(as.Date("2012-11-02") - as.Date("2012-01-10")))
})

test_that("2. Pending case", {
  df <- data.frame(
    filing_date = as.Date("2012-06-20"),
    decision_date = as.Date(NA)
  )
  res <- clean_cases(df, observation_end = "2014-12-31")

  expect_true(res$is_valid)
  expect_true(is.na(res$invalid_reason))
  expect_equal(res$event, 0)
  expect_equal(res$duration, as.numeric(as.Date("2014-12-31") - as.Date("2012-06-20")))
})

test_that("3. Decision before filing date", {
  df <- data.frame(
    filing_date = as.Date("2013-05-30"),
    decision_date = as.Date("2012-05-30")
  )
  res <- clean_cases(df, observation_end = "2014-12-31")

  expect_false(res$is_valid)
  expect_equal(res$invalid_reason, "Decision date before filing date")
  expect_true(is.na(res$duration))
  expect_true(is.na(res$event))
})

test_that("4. Missing filing date", {
  df <- data.frame(
    filing_date = as.Date(NA),
    decision_date = as.Date("2013-05-30")
  )
  res <- clean_cases(df, observation_end = "2014-12-31")

  expect_false(res$is_valid)
  expect_equal(res$invalid_reason, "Missing filing date")
  expect_true(is.na(res$duration))
  expect_true(is.na(res$event))
})

test_that("5. Case filed after observation_end", {
  df <- data.frame(
    filing_date = as.Date("2015-02-01"),
    decision_date = as.Date(NA)
  )
  res <- clean_cases(df, observation_end = "2014-12-31")

  expect_false(res$is_valid)
  expect_equal(res$invalid_reason, "Filing date after observation_end")
  expect_true(is.na(res$duration))
  expect_true(is.na(res$event))
})

test_that("6. Zero-day duration", {
  df <- data.frame(
    filing_date = as.Date("2012-01-10"),
    decision_date = as.Date("2012-01-10")
  )
  res <- clean_cases(df, observation_end = "2014-12-31")

  expect_true(res$is_valid)
  expect_equal(res$duration, 0)
  expect_equal(res$event, 1)
})

test_that("7. Multiple cases with mixed disposed/pending/invalid status", {
  df <- data.frame(
    case_id = 1:5,
    filing_date = as.Date(c("2012-01-10", "2012-03-05", "2013-05-30", NA, "2015-01-01")),
    decision_date = as.Date(c("2012-11-02", NA, "2012-05-30", "2013-01-01", NA))
  )
  res <- clean_cases(df, observation_end = "2014-12-31")

  expect_equal(nrow(res), 5)
  expect_equal(res$is_valid, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(res$event[1:2], c(1, 0))
  expect_true(all(is.na(res$event[3:5])))
})

test_that("8. Correct event values and 9. Correct duration values", {
  syn <- get_synthetic_cases()
  res <- clean_cases(syn, observation_end = "2014-12-31")

  expect_equal(res$event, c(1, 1, 0, 1, 0, 0))
  expect_equal(res$duration, c(297, 346, 924, 293, 580, 350))
})

test_that("10. No negative durations across any dataset", {
  df <- data.frame(
    filing_date = as.Date(c("2012-01-10", "2013-05-30", "2015-01-01")),
    decision_date = as.Date(c("2012-01-10", "2012-01-01", NA))
  )
  res <- clean_cases(df, observation_end = "2014-12-31")

  valid_durations <- res$duration[!is.na(res$duration)]
  expect_true(all(valid_durations >= 0))
})

test_that("Invalid input handling", {
  expect_error(clean_cases("not a df", observation_end = "2014-12-31"))
  expect_error(clean_cases(get_synthetic_cases()))
  expect_error(clean_cases(get_synthetic_cases(), observation_end = "invalid-date-string"))
})
