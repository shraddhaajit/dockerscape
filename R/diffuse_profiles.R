#' Diffuse Case Legal Profiles Over Section Co-occurrence Graph
#'
#' Performs a one-step personalized diffusion of case-level legal section profiles
#' across a weighted section co-occurrence graph. This allows legally related cases
#' to share similarity even when they do not invoke identical sections.
#'
#' @param sections A \code{data.frame} containing case section records (must include case ID, Act, and Section columns).
#' @param section_graph An \code{igraph} graph object representing legal section co-occurrence (e.g. created by \code{\link{build_section_graph}}).
#' @param case Character string specifying the column name for case IDs. Defaults to \code{"case_id"}.
#' @param act Character string specifying the column name for Act names. Defaults to \code{"act"}.
#' @param section Character string specifying the column name for Section numbers/names. Defaults to \code{"section"}.
#' @param alpha Numeric value in \code{[0, 1]} controlling the weight retained by the original observed section profile. Defaults to \code{0.7}.
#'
#' @return An S3 object of class \code{"diffused_profiles"} containing:
#' \describe{
#'   \item{profiles}{A sparse matrix of class \code{dgCMatrix} (from \code{Matrix}) of dimension \code{N_cases x M_sections}, with row sums equal to 1.}
#'   \item{case_ids}{Character vector of unique case IDs corresponding to matrix rows.}
#'   \item{section_ids}{Character vector of section identifiers (\code{"Act::Section"}) corresponding to matrix columns.}
#'   \item{alpha}{The numeric alpha value used for diffusion.}
#' }
#'
#' @details
#' \bold{Mathematical Formulation:}
#' Let \eqn{A} be the weighted adjacency matrix of the section graph, and \eqn{D} be the diagonal degree matrix.
#' The transition matrix is defined as \eqn{P = D^{-1} A} (with rows of isolated nodes set to 0).
#'
#' For a case with normalized initial section indicator vector \eqn{x_{init}} (where \eqn{\|x_{init}\|_1 = 1}):
#' \deqn{x_{raw} = \alpha \cdot x_{init} + (1 - \alpha) \cdot (x_{init} P)}
#'
#' The final profile is normalized to unit \eqn{L_1} norm:
#' \deqn{x_{final} = \frac{x_{raw}}{\|x_{raw}\|_1}}
#'
#' If \eqn{\alpha = 1}, the original section profile is returned without diffusion.
#' If \eqn{\alpha = 0}, the profile is derived entirely from neighboring sections in the co-occurrence graph.
#'
#' @examples
#' syn_sec <- data.frame(
#'   case_id = c(1, 1, 2, 2, 3),
#'   act = c("NI Act", "NI Act", "NI Act", "NI Act", "IPC"),
#'   section = c("138", "141", "138", "142", "302")
#' )
#' g <- build_section_graph(syn_sec)
#' diff_res <- diffuse_profiles(syn_sec, g, alpha = 0.7)
#' print(diff_res)
#'
#' @export
diffuse_profiles <- function(sections,
                             section_graph,
                             case = "case_id",
                             act = "act",
                             section = "section",
                             alpha = 0.7) {
  if (!is.data.frame(sections)) {
    stop("`sections` must be a data.frame.", call. = FALSE)
  }

  if (!inherits(section_graph, "igraph")) {
    stop("`section_graph` must be an igraph object.", call. = FALSE)
  }

  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1) {
    stop("`alpha` must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  req_cols <- c(case, act, section)
  missing_cols <- setdiff(req_cols, names(sections))
  if (length(missing_cols) > 0) {
    stop(sprintf("Column(s) not found in `sections`: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  case_vals <- sections[[case]]
  act_vals <- as.character(sections[[act]])
  sec_vals <- as.character(sections[[section]])

  # Filter valid non-NA non-empty rows
  valid_mask <- !is.na(case_vals) &
                !is.na(act_vals) & nchar(trimws(act_vals)) > 0 &
                !is.na(sec_vals) & nchar(trimws(sec_vals)) > 0

  if (!any(valid_mask)) {
    # Handle empty input
    graph_sec_ids <- igraph::V(section_graph)$name
    if (is.null(graph_sec_ids)) graph_sec_ids <- character(0)
    empty_mat <- Matrix::Matrix(0, nrow = 0, ncol = length(graph_sec_ids), sparse = TRUE)
    colnames(empty_mat) <- graph_sec_ids
    res <- list(
      profiles = empty_mat,
      case_ids = character(0),
      section_ids = graph_sec_ids,
      alpha = alpha
    )
    class(res) <- "diffused_profiles"
    return(res)
  }

  clean_df <- data.frame(
    case_id = as.character(case_vals[valid_mask]),
    act = trimws(act_vals[valid_mask]),
    section = trimws(sec_vals[valid_mask]),
    stringsAsFactors = FALSE
  )
  clean_df$identifier <- paste0(clean_df$act, "::", clean_df$section)

  # Deduplicate case-section memberships
  unique_df <- unique(clean_df[, c("case_id", "identifier")])

  # Unique case IDs sorted naturally/character-wise
  unique_cases <- unique(unique_df$case_id)
  suppressWarnings(num_cases <- as.numeric(unique_cases))
  if (!any(is.na(num_cases))) {
    unique_cases <- unique_cases[order(num_cases)]
  } else {
    unique_cases <- sort(unique_cases)
  }
  n_cases <- length(unique_cases)

  # Graph vertex section IDs
  graph_sec_ids <- igraph::V(section_graph)$name
  if (is.null(graph_sec_ids)) graph_sec_ids <- character(0)

  # Any section IDs present in sections data but missing from graph
  data_sec_ids <- unique(unique_df$identifier)
  extra_sec_ids <- setdiff(data_sec_ids, graph_sec_ids)

  all_sec_ids <- c(graph_sec_ids, extra_sec_ids)
  m_sections <- length(all_sec_ids)

  if (m_sections == 0) {
    empty_mat <- Matrix::Matrix(0, nrow = n_cases, ncol = 0, sparse = TRUE)
    rownames(empty_mat) <- unique_cases
    res <- list(
      profiles = empty_mat,
      case_ids = unique_cases,
      section_ids = character(0),
      alpha = alpha
    )
    class(res) <- "diffused_profiles"
    return(res)
  }

  # Build initial sparse binary matrix X_init (n_cases x m_sections)
  case_idx <- match(unique_df$case_id, unique_cases)
  sec_idx <- match(unique_df$identifier, all_sec_ids)

  X_raw <- Matrix::sparseMatrix(
    i = case_idx,
    j = sec_idx,
    x = 1,
    dims = c(n_cases, m_sections),
    dimnames = list(unique_cases, all_sec_ids)
  )

  # Normalize initial profile per case (L1 norm = 1)
  initial_row_sums <- Matrix::rowSums(X_raw)
  initial_row_sums[initial_row_sums == 0] <- 1
  X_init <- Matrix::Diagonal(x = 1 / initial_row_sums) %*% X_raw
  dimnames(X_init) <- list(unique_cases, all_sec_ids)

  if (alpha == 1) {
    res <- list(
      profiles = X_init,
      case_ids = unique_cases,
      section_ids = all_sec_ids,
      alpha = alpha
    )
    class(res) <- "diffused_profiles"
    return(res)
  }

  # Construct Transition Matrix P (m_sections x m_sections)
  if (igraph::vcount(section_graph) > 0) {
    A_graph <- igraph::as_adjacency_matrix(section_graph, attr = "weight", sparse = TRUE)
    A_graph <- A_graph[graph_sec_ids, graph_sec_ids, drop = FALSE]
  } else {
    A_graph <- Matrix::Matrix(0, nrow = 0, ncol = 0, sparse = TRUE)
  }

  # Pad A_graph if extra_sec_ids exist
  if (length(extra_sec_ids) > 0) {
    A_full <- Matrix::Matrix(0, nrow = m_sections, ncol = m_sections, sparse = TRUE)
    if (length(graph_sec_ids) > 0) {
      A_full[1:length(graph_sec_ids), 1:length(graph_sec_ids)] <- A_graph
    }
    dimnames(A_full) <- list(all_sec_ids, all_sec_ids)
  } else {
    A_full <- A_graph
  }

  deg <- Matrix::rowSums(A_full)
  inv_deg <- ifelse(deg > 0, 1 / deg, 0)
  D_inv <- Matrix::Diagonal(x = inv_deg)
  P <- D_inv %*% A_full

  # Perform one-step diffusion: X_raw_diff = alpha * X_init + (1 - alpha) * (X_init %*% P)
  X_diff_raw <- alpha * X_init + (1 - alpha) * (X_init %*% P)

  # Normalize final profiles so row sums equal 1
  final_row_sums <- Matrix::rowSums(X_diff_raw)
  zero_rows <- (final_row_sums == 0)
  if (any(zero_rows)) {
    X_diff_raw[zero_rows, ] <- X_init[zero_rows, ]
    final_row_sums[zero_rows] <- 1
  }

  X_final <- Matrix::Diagonal(x = 1 / final_row_sums) %*% X_diff_raw
  dimnames(X_final) <- list(unique_cases, all_sec_ids)

  res <- list(
    profiles = X_final,
    case_ids = unique_cases,
    section_ids = all_sec_ids,
    alpha = alpha
  )
  class(res) <- "diffused_profiles"
  res
}

#' Print method for diffused_profiles
#' @param x A \code{diffused_profiles} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print diffused_profiles
print.diffused_profiles <- function(x, ...) {
  cat("Diffused Case Legal Profiles\n")
  cat("============================\n")
  cat("Number of cases:    ", length(x$case_ids), "\n")
  cat("Number of sections: ", length(x$section_ids), "\n")
  cat("Alpha parameter:    ", x$alpha, "\n")
  cat("Sparse matrix size: ", dim(x$profiles)[1], "x", dim(x$profiles)[2], "\n")
  invisible(x)
}
