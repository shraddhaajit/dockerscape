test_that("1. Correct number of unique legal-section nodes", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  # Unique section identifiers: NI Act::138, NI Act::141, NI Act::142, IPC::302, IPC::420 -> 5 nodes
  expect_equal(igraph::vcount(g), 5)
  expect_setequal(
    igraph::V(g)$identifier,
    c("NI Act::138", "NI Act::141", "NI Act::142", "IPC::302", "IPC::420")
  )
})

test_that("2. NI Act 138 and NI Act 141 are connected", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  expect_true(igraph::are_adjacent(g, "NI Act::138", "NI Act::141"))
})

test_that("3. Edge weight between NI Act 138 and NI Act 141 is correct", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  # Co-occurs in Case 1 and Case 4 -> weight = 2
  e_weight <- igraph::E(g)["NI Act::138" %--% "NI Act::141"]$weight
  expect_equal(e_weight, 2)
})

test_that("4. NI Act 138 and NI Act 142 are connected with correct weight", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  expect_true(igraph::are_adjacent(g, "NI Act::138", "NI Act::142"))
  e_weight <- igraph::E(g)["NI Act::138" %--% "NI Act::142"]$weight
  expect_equal(e_weight, 1)
})

test_that("5. IPC 302 and IPC 420 are NOT connected in README example", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  expect_false(igraph::are_adjacent(g, "IPC::302", "IPC::420"))
})

test_that("6. NI Act 138 and IPC 138 are separate nodes", {
  sec <- data.frame(
    case_id = c(1, 1),
    act = c("NI Act", "IPC"),
    section = c("138", "138"),
    stringsAsFactors = FALSE
  )
  g <- build_section_graph(sec)

  expect_equal(igraph::vcount(g), 2)
  expect_setequal(igraph::V(g)$identifier, c("NI Act::138", "IPC::138"))
})

test_that("7. Duplicate case-section rows do not inflate edge weights", {
  sec <- data.frame(
    case_id = c(1, 1, 1, 1),
    act = c("NI Act", "NI Act", "NI Act", "NI Act"),
    section = c("138", "138", "141", "141"),
    stringsAsFactors = FALSE
  )
  g <- build_section_graph(sec)

  e_weight <- igraph::E(g)["NI Act::138" %--% "NI Act::141"]$weight
  expect_equal(e_weight, 1)
})

test_that("8. A section appearing in only one case is retained as a node", {
  sec <- data.frame(
    case_id = c(1, 2),
    act = c("NI Act", "IPC"),
    section = c("138", "302"),
    stringsAsFactors = FALSE
  )
  g <- build_section_graph(sec)

  expect_equal(igraph::vcount(g), 2)
  expect_equal(igraph::ecount(g), 0)
  expect_setequal(igraph::V(g)$identifier, c("NI Act::138", "IPC::302"))
})

test_that("9. Missing case_id/act/section is handled properly", {
  sec <- data.frame(
    case_id = c(1, 1, 2, NA),
    act = c("NI Act", "NI Act", NA, "IPC"),
    section = c("138", "141", "138", NA),
    stringsAsFactors = FALSE
  )
  g <- build_section_graph(sec)

  # Only valid row pair is case 1: NI Act::138 & NI Act::141
  expect_equal(igraph::vcount(g), 2)
  expect_setequal(igraph::V(g)$identifier, c("NI Act::138", "NI Act::141"))
})

test_that("10. Empty input is handled explicitly", {
  sec_empty <- data.frame(case_id = numeric(0), act = character(0), section = character(0))
  g <- build_section_graph(sec_empty)

  expect_true(inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), 0)
  expect_equal(igraph::ecount(g), 0)
})

test_that("11. Output is an igraph graph", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  expect_true(inherits(g, "igraph"))
  expect_false(igraph::is_directed(g))
})

test_that("12. Edge weights are numeric and non-negative", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  weights <- igraph::E(g)$weight
  expect_true(is.numeric(weights))
  expect_true(all(weights >= 0))
})

test_that("13. Node attributes act, section, and identifier exist", {
  syn_sec <- get_synthetic_sections()
  g <- build_section_graph(syn_sec)

  expect_true("act" %in% igraph::vertex_attr_names(g))
  expect_true("section" %in% igraph::vertex_attr_names(g))
  expect_true("identifier" %in% igraph::vertex_attr_names(g))
})

test_that("14. Re-running on same input yields identical graph structure and weights", {
  syn_sec <- get_synthetic_sections()
  g1 <- build_section_graph(syn_sec)
  g2 <- build_section_graph(syn_sec)

  expect_equal(igraph::vcount(g1), igraph::vcount(g2))
  expect_equal(igraph::ecount(g1), igraph::ecount(g2))
  expect_equal(igraph::V(g1)$identifier, igraph::V(g2)$identifier)
  expect_equal(igraph::E(g1)$weight, igraph::E(g2)$weight)
})
