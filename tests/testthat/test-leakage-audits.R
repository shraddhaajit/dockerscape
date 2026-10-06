test_that("1. check_future_neighbours directionality and cutoff audit", {
  cases_df <- data.frame(
    case_id = c("A", "B", "C"),
    filing_date = as.Date(c("2012-01-10", "2013-02-15", "2012-03-01")),
    stringsAsFactors = FALSE
  )
  # A (2012-01-10) connected to B (2013-02-15) with weight 0.8
  # A (2012-01-10) connected to C (2012-03-01) with weight 0.5
  el <- data.frame(
    from = c("A", "A"),
    to = c("B", "C"),
    weight = c(0.8, 0.5),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(el, directed = FALSE)

  # Audit with cutoff 2012-06-01:
  # A and C are pre-cutoff (<= 2012-06-01); B is post-cutoff (> 2012-06-01).
  audit_cutoff <- check_future_neighbours(g, cases_df, cutoff_date = as.Date("2012-06-01"))

  expect_s3_class(audit_cutoff, "future_neighbour_audit")
  expect_true(audit_cutoff$available)
  expect_equal(audit_cutoff$n_edges_checked, 2)
  expect_equal(audit_cutoff$n_future_neighbour_edges, 1) # Only A -> B edge
  expect_equal(audit_cutoff$n_cases_with_future_neighbours, 1)
  expect_equal(audit_cutoff$affected_case_ids, "A")

  df_leak <- audit_cutoff$future_neighbour_edges
  expect_equal(df_leak$case_id, "A")
  expect_equal(df_leak$neighbour_id, "B")
  expect_equal(df_leak$edge_weight, 0.8)
  expect_equal(df_leak$case_filing_date, as.Date("2012-01-10"))
  expect_equal(df_leak$neighbour_filing_date, as.Date("2013-02-15"))
  expect_equal(df_leak$temporal_gap_days, as.integer(as.Date("2013-02-15") - as.Date("2012-01-10")))

  # Reverse direction check: B does NOT have A as a future neighbour
  expect_false("B" %in% df_leak$case_id)

  # Audit with cutoff = NULL (Full graph ordering):
  # A -> B (gap 401 days), A -> C (gap 51 days), C -> A (not future)
  audit_null <- check_future_neighbours(g, cases_df, cutoff_date = NULL)
  expect_equal(audit_null$n_future_neighbour_edges, 2) # Both A->B and A->C have neighbour filed later
})

test_that("2. check_future_neighbours edge cases (missing dates, isolated nodes, no edges)", {
  empty_g <- igraph::make_empty_graph(n = 0, directed = FALSE)
  cases_df <- data.frame(case_id = "A", filing_date = as.Date("2012-01-01"))

  audit_empty <- check_future_neighbours(empty_g, cases_df)
  expect_equal(audit_empty$n_edges_checked, 0)
  expect_equal(audit_empty$n_future_neighbour_edges, 0)

  # Unknown graph node handling
  g_unk <- igraph::graph_from_data_frame(data.frame(from = "A", to = "UNKNOWN", weight = 0.5), directed = FALSE)
  audit_unk <- check_future_neighbours(g_unk, cases_df)
  expect_equal(audit_unk$n_future_neighbour_edges, 0) # UNKNOWN has NA filing date, skipped safely
})

test_that("3. check_court_overlap auditing", {
  tr_df <- data.frame(court = c("C1", "C1", "C2", "C3"))
  val_df <- data.frame(court = c("C2", "C3", "C4"))

  audit <- check_court_overlap(tr_df, val_df)

  expect_s3_class(audit, "court_overlap_audit")
  expect_equal(audit$training_courts, c("C1", "C2", "C3"))
  expect_equal(audit$validation_courts, c("C2", "C3", "C4"))
  expect_equal(audit$overlapping_courts, c("C2", "C3"))
  expect_equal(audit$n_overlapping_courts, 2)
  expect_equal(audit$training_only_courts, "C1")
  expect_equal(audit$validation_only_courts, "C4")
  expect_equal(audit$overlap_ratio, 2/3)

  # No overlap scenario
  audit_none <- check_court_overlap(c("C1", "C2"), c("C3", "C4"))
  expect_equal(audit_none$n_overlapping_courts, 0)
  expect_equal(audit_none$overlap_ratio, 0)
})

test_that("4. check_near_duplicates exact duplicate detection", {
  df <- data.frame(
    case_id = 1:5,
    court = c("C1", "C1", "C1", "C2", "C2"),
    filing_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-02-01", "2020-03-01")),
    decision_date = as.Date(c("2020-06-01", "2020-06-01", "2020-06-01", NA, NA)),
    stringsAsFactors = FALSE
  )

  audit <- check_near_duplicates(df)

  expect_s3_class(audit, "near_duplicate_audit")
  expect_true(audit$available)
  expect_equal(audit$n_rows_checked, 5)
  expect_equal(audit$n_exact_duplicate_groups, 1)
  expect_equal(audit$n_rows_in_duplicate_groups, 3)

  # No duplicates scenario
  df_no_dup <- data.frame(
    case_id = 1:3,
    court = c("C1", "C2", "C3"),
    filing_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    decision_date = as.Date(c("2020-06-01", "2020-07-01", "2020-08-01"))
  )
  audit_no_dup <- check_near_duplicates(df_no_dup)
  expect_equal(audit_no_dup$n_exact_duplicate_groups, 0)
  expect_equal(audit_no_dup$n_rows_in_duplicate_groups, 0)
})
