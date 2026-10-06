test_that("1. Manual mathematical verification on tiny graph", {
  # Graph vertices: A::1, B::1, C::1
  # Edges: A::1 -- B::1 (weight 1), A::1 -- C::1 (weight 1)
  # Degree of A::1 = 2, so transition probabilities from A::1 are 0.5 to B::1, 0.5 to C::1.
  sec_tiny <- data.frame(
    case_id = c(1, 1, 2, 2),
    act = c("A", "B", "A", "C"),
    section = c("1", "1", "1", "1"),
    stringsAsFactors = FALSE
  )
  g_tiny <- build_section_graph(sec_tiny)

  # Target Case: Case 3 with only section A::1
  sec_case3 <- data.frame(
    case_id = c(3),
    act = c("A"),
    section = c("1"),
    stringsAsFactors = FALSE
  )

  # Run diffuse_profiles with alpha = 0.7
  diff_res <- diffuse_profiles(sec_case3, g_tiny, alpha = 0.7)

  # Manual expectation:
  # Initial profile x_init = (A::1 = 1, B::1 = 0, C::1 = 0)
  # x_init %*% P = (A::1 = 0, B::1 = 0.5, C::1 = 0.5)
  # x_diff = 0.7 * (1, 0, 0) + 0.3 * (0, 0.5, 0.5) = (0.7, 0.15, 0.15)
  # Sum = 1.0 -> Normalized = (0.7, 0.15, 0.15)
  prof_c3 <- diff_res$profiles["3", c("A::1", "B::1", "C::1")]
  expect_equal(as.numeric(prof_c3), c(0.7, 0.15, 0.15))
})

test_that("2. Output structure and class", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)
  res <- diffuse_profiles(syn_sec, g, alpha = 0.7)

  expect_s3_class(res, "diffused_profiles")
  expect_true(inherits(res$profiles, "dgCMatrix"))
  expect_equal(res$alpha, 0.7)
  expect_true(is.character(res$case_ids))
  expect_true(is.character(res$section_ids))
})

test_that("3. Case IDs preserved and section IDs match section_graph", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)
  res <- diffuse_profiles(syn_sec, g, alpha = 0.7)

  expect_setequal(res$case_ids, as.character(unique(syn_sec$case_id)))
  expect_equal(res$section_ids, igraph::V(g)$name)
  expect_equal(rownames(res$profiles), res$case_ids)
  expect_equal(colnames(res$profiles), res$section_ids)
})

test_that("4. Alpha parameter validation", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  expect_error(diffuse_profiles(syn_sec, g, alpha = -0.1))
  expect_error(diffuse_profiles(syn_sec, g, alpha = 1.1))
  expect_error(diffuse_profiles(syn_sec, g, alpha = "invalid"))
  expect_error(diffuse_profiles(syn_sec, g, alpha = c(0.5, 0.7)))
})

test_that("5. Alpha = 1 identity behavior", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)
  res_alpha1 <- diffuse_profiles(syn_sec, g, alpha = 1.0)

  # Case 1 has 2 sections: NI Act::138 and NI Act::141
  # Initial normalized profile for Case 1 should be 0.5 for each, 0 elsewhere
  c1_prof <- res_alpha1$profiles["1", ]
  expect_equal(as.numeric(c1_prof["NI Act::138"]), 0.5)
  expect_equal(as.numeric(c1_prof["NI Act::141"]), 0.5)
  expect_equal(as.numeric(c1_prof["NI Act::142"]), 0)
  expect_equal(as.numeric(c1_prof["IPC::302"]), 0)
})

test_that("6. Alpha = 0 neighbor-only diffusion behavior", {
  sec_tiny <- data.frame(
    case_id = c(1, 1),
    act = c("NI Act", "NI Act"),
    section = c("138", "141"),
    stringsAsFactors = FALSE
  )
  g_tiny <- build_section_graph(sec_tiny)

  # Case 2 has only NI Act::138.
  sec_c2 <- data.frame(
    case_id = 2,
    act = "NI Act",
    section = "138",
    stringsAsFactors = FALSE
  )

  res_alpha0 <- diffuse_profiles(sec_c2, g_tiny, alpha = 0.0)

  # At alpha = 0, mass moves entirely to NI Act::141 (the neighbor of 138)
  c2_prof <- res_alpha0$profiles["2", ]
  expect_equal(as.numeric(c2_prof["NI Act::138"]), 0)
  expect_equal(as.numeric(c2_prof["NI Act::141"]), 1)
})

test_that("7. Isolated-node behavior", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  # Case 3 has IPC::302 which is isolated in the graph
  res <- diffuse_profiles(syn_sec, g, alpha = 0.7)

  c3_prof <- res$profiles["3", ]
  expect_equal(as.numeric(c3_prof["IPC::302"]), 1)
  expect_equal(sum(c3_prof), 1)
})

test_that("8. Duplicate-row handling", {
  sec_dup <- data.frame(
    case_id = c(1, 1, 1, 1),
    act = c("NI Act", "NI Act", "NI Act", "NI Act"),
    section = c("138", "138", "141", "141"),
    stringsAsFactors = FALSE
  )
  g <- build_section_graph(sec_dup)
  res <- diffuse_profiles(sec_dup, g, alpha = 1.0)

  c1_prof <- res$profiles["1", ]
  expect_equal(as.numeric(c1_prof["NI Act::138"]), 0.5)
  expect_equal(as.numeric(c1_prof["NI Act::141"]), 0.5)
})

test_that("9. Profile normalization holds across all cases", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)
  res <- diffuse_profiles(syn_sec, g, alpha = 0.7)

  row_sums <- Matrix::rowSums(res$profiles)
  expect_equal(as.numeric(row_sums), rep(1.0, length(row_sums)))
})

test_that("10. Non-negative profile values", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)
  res <- diffuse_profiles(syn_sec, g, alpha = 0.7)

  expect_true(all(res$profiles >= 0))
})

test_that("11. Reproducibility", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)
  res1 <- diffuse_profiles(syn_sec, g, alpha = 0.7)
  res2 <- diffuse_profiles(syn_sec, g, alpha = 0.7)

  expect_equal(res1$profiles, res2$profiles)
  expect_equal(res1$case_ids, res2$case_ids)
  expect_equal(res1$section_ids, res2$section_ids)
})

test_that("12. Missing-data behavior", {
  sec_missing <- data.frame(
    case_id = c(1, 1, 2, NA),
    act = c("NI Act", "NI Act", NA, "IPC"),
    section = c("138", "141", "138", NA),
    stringsAsFactors = FALSE
  )
  g <- build_section_graph(sec_missing)
  res <- diffuse_profiles(sec_missing, g, alpha = 0.7)

  # Only case 1 has valid complete section data
  expect_equal(res$case_ids, "1")
  expect_equal(nrow(res$profiles), 1)
})

test_that("13. Empty input behavior", {
  sec_empty <- data.frame(case_id = numeric(0), act = character(0), section = character(0))
  g_empty <- build_section_graph(sec_empty)
  res <- diffuse_profiles(sec_empty, g_empty, alpha = 0.7)

  expect_equal(nrow(res$profiles), 0)
  expect_equal(length(res$case_ids), 0)
})

test_that("14. Un-graphed section handling", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  # Supply a case with a section not in the section_graph
  sec_extra <- data.frame(
    case_id = 99,
    act = "CustomAct",
    section = "999",
    stringsAsFactors = FALSE
  )

  res <- diffuse_profiles(sec_extra, g, alpha = 0.7)

  expect_true("CustomAct::999" %in% res$section_ids)
  expect_equal(as.numeric(res$profiles["99", "CustomAct::999"]), 1.0)
})
