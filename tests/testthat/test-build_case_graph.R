test_that("1. Two identical profiles receive similarity 1", {
  mat <- Matrix::Matrix(c(1, 0, 1, 0), nrow = 2, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B"), section_ids = c("S1", "S2"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  g <- build_case_graph(prof, k = 1, use_idf = FALSE)

  expect_equal(igraph::ecount(g), 1)
  expect_equal(igraph::E(g)$weight[1], 1.0)
})

test_that("2. Orthogonal profiles receive similarity 0 (no edge)", {
  mat <- Matrix::Matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B"), section_ids = c("S1", "S2"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  g <- build_case_graph(prof, k = 1, use_idf = FALSE)

  expect_equal(igraph::ecount(g), 0)
})

test_that("3. Three cases with known relative similarities", {
  # Case A: (1, 0), Case B: (1, 1), Case C: (0, 1)
  # Cosine A-B: 1/sqrt(2) ≈ 0.7071
  # Cosine B-C: 1/sqrt(2) ≈ 0.7071
  # Cosine A-C: 0
  mat <- Matrix::Matrix(c(1, 0, 1, 1, 0, 1), nrow = 3, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B", "C"), section_ids = c("S1", "S2"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  g <- build_case_graph(prof, k = 2, use_idf = FALSE)

  expect_true(igraph::are_adjacent(g, "A", "B"))
  expect_true(igraph::are_adjacent(g, "B", "C"))
  expect_false(igraph::are_adjacent(g, "A", "C"))
  e_ab <- igraph::E(g)["A" %--% "B"]$weight
  expect_equal(round(e_ab, 4), 0.7071)
})

test_that("4. k larger than number of possible neighbours retains all non-zero neighbours", {
  mat <- Matrix::Matrix(c(1, 1, 1, 0, 0, 1), nrow = 3, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B", "C"), section_ids = c("S1", "S2"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  # k = 10, N = 3
  g <- build_case_graph(prof, k = 10, use_idf = FALSE)

  expect_equal(igraph::vcount(g), 3)
  expect_true(inherits(g, "igraph"))
})

test_that("5. k = 1 limits nearest neighbours correctly", {
  # Case A: (1, 1, 0), Case B: (1, 0, 0), Case C: (0, 0, 1)
  # A-B sim > 0, A-C sim = 0
  mat <- Matrix::Matrix(c(1, 1, 0, 1, 0, 0, 0, 0, 1), nrow = 3, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B", "C"), section_ids = c("S1", "S2", "S3"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  g <- build_case_graph(prof, k = 1, use_idf = FALSE)

  expect_equal(igraph::ecount(g), 1)
  expect_true(igraph::are_adjacent(g, "A", "B"))
})

test_that("6. Invalid k values are rejected", {
  mat <- Matrix::Matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B"), section_ids = c("S1", "S2"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  expect_error(build_case_graph(prof, k = 0))
  expect_error(build_case_graph(prof, k = -2))
  expect_error(build_case_graph(prof, k = 1.5))
  expect_error(build_case_graph(prof, k = "invalid"))
})

test_that("7. Duplicate case IDs are rejected", {
  mat <- Matrix::Matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "A"), section_ids = c("S1", "S2"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  expect_error(build_case_graph(prof))
})

test_that("8. Zero profile is handled explicitly", {
  # Case B has all zero profile
  mat <- Matrix::Matrix(c(1, 0, 0, 0), nrow = 2, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B"), section_ids = c("S1", "S2"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  g <- build_case_graph(prof, k = 1, use_idf = FALSE)

  expect_equal(igraph::vcount(g), 2)
  expect_equal(igraph::ecount(g), 0)
})

test_that("9. Identical similarity ties are broken deterministically", {
  # Case A: (1, 1, 1), Case B: (1, 0, 0), Case C: (1, 0, 0)
  # Sim(A, B) = Sim(A, C) = 1/sqrt(3)
  mat <- Matrix::Matrix(c(1, 1, 1, 1, 0, 0, 1, 0, 0), nrow = 3, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B", "C"), section_ids = c("S1", "S2", "S3"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  g1 <- build_case_graph(prof, k = 1, use_idf = FALSE)
  g2 <- build_case_graph(prof, k = 1, use_idf = FALSE)

  expect_equal(igraph::E(g1)$weight, igraph::E(g2)$weight)
  expect_equal(igraph::as_edgelist(g1), igraph::as_edgelist(g2))
})

test_that("10. Self-edges are absent", {
  syn_sec <- get_synthetic_sections()
  sec_g <- build_section_graph(syn_sec)
  diff_p <- diffuse_profiles(syn_sec, sec_g)

  g <- build_case_graph(diff_p, k = 3)

  expect_false(any(igraph::which_loop(g)))
})

test_that("11. Graph is undirected", {
  syn_sec <- get_synthetic_sections()
  sec_g <- build_section_graph(syn_sec)
  diff_p <- diffuse_profiles(syn_sec, sec_g)

  g <- build_case_graph(diff_p, k = 3)

  expect_false(igraph::is_directed(g))
})

test_that("12. Case IDs are preserved", {
  syn_sec <- get_synthetic_sections()
  sec_g <- build_section_graph(syn_sec)
  diff_p <- diffuse_profiles(syn_sec, sec_g)

  g <- build_case_graph(diff_p, k = 3)

  expect_equal(igraph::V(g)$name, diff_p$case_ids)
  expect_equal(igraph::V(g)$case_id, diff_p$case_ids)
})

test_that("13. Edge weights are numeric and lie in [0, 1]", {
  syn_sec <- get_synthetic_sections()
  sec_g <- build_section_graph(syn_sec)
  diff_p <- diffuse_profiles(syn_sec, sec_g)

  g <- build_case_graph(diff_p, k = 3)

  w <- igraph::E(g)$weight
  expect_true(is.numeric(w))
  expect_true(all(w >= 0 & w <= 1.0))
})

test_that("14. Rarity (IDF) weighting gives greater weight to rare sections", {
  # 4 cases:
  # Section S1 appears in 4 cases (common)
  # Section S2 appears in 1 case (rare: Case D)
  # Case A: (1, 0), Case B: (1, 0), Case C: (1, 0), Case D: (1, 1)
  # Case D has both common S1 and rare S2.
  # IDF: S1 = log(4/4) + 1 = 1.0; S2 = log(4/1) + 1 ≈ 2.3863.
  mat <- Matrix::Matrix(c(
    1, 0,
    1, 0,
    1, 0,
    1, 1
  ), nrow = 4, byrow = TRUE, sparse = TRUE)
  prof <- list(profiles = mat, case_ids = c("A", "B", "C", "D"), section_ids = c("S1", "S2"), alpha = 1.0)
  class(prof) <- "diffused_profiles"

  g_idf <- build_case_graph(prof, k = 3, use_idf = TRUE)
  g_noidf <- build_case_graph(prof, k = 3, use_idf = FALSE)

  sim_idf <- igraph::E(g_idf)["A" %--% "D"]$weight
  sim_noidf <- igraph::E(g_noidf)["A" %--% "D"]$weight

  expect_equal(round(sim_idf, 4), 0.3865)
  expect_equal(round(sim_noidf, 4), 0.7071)
})

test_that("15. Duplicate section records do not alter case-level rarity", {
  syn_sec <- get_synthetic_sections()
  sec_g <- build_section_graph(syn_sec)
  diff_p <- diffuse_profiles(syn_sec, sec_g)

  g1 <- build_case_graph(diff_p, k = 3, use_idf = TRUE)
  g2 <- build_case_graph(diff_p, k = 3, use_idf = TRUE)

  expect_equal(igraph::E(g1)$weight, igraph::E(g2)$weight)
})

test_that("16. Empty input returns empty graph gracefully", {
  prof_empty <- list(
    profiles = Matrix::Matrix(0, nrow = 0, ncol = 0, sparse = TRUE),
    case_ids = character(0),
    section_ids = character(0),
    alpha = 0.7
  )
  class(prof_empty) <- "diffused_profiles"

  g <- build_case_graph(prof_empty)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 0)
  expect_equal(igraph::ecount(g), 0)
})
