#' Construct a Weighted Case Similarity Graph
#'
#' Builds an undirected \code{igraph} graph of cases where nodes represent cases,
#' edges connect legally similar cases based on their diffused section profiles, and
#' edge weights represent cosine legal similarity values in \code{[0, 1]} after optional
#' inverse-document-frequency (IDF) section rarity weighting.
#'
#' @param profiles An object of class \code{\link{diffuse_profiles}} containing diffused case profiles.
#' @param k Positive integer specifying the maximum number of nearest neighbours each case retains. Defaults to \code{3}.
#' @param use_idf Logical flag indicating whether to weight sections by inverse document frequency (rarity) prior to cosine similarity. Defaults to \code{TRUE}.
#'
#' @return An undirected \code{igraph} graph object containing:
#' \describe{
#'   \item{Vertex attributes}{\code{name} / \code{case_id} matching the input case IDs.}
#'   \item{Edge attributes}{\code{weight} numeric cosine legal similarity values in \code{[0, 1]}.}
#' }
#'
#' @details
#' \bold{Graph Construction Pipeline:}
#' \enumerate{
#'   \item \bold{IDF Rarity Weighting:} For section \eqn{s} present in \eqn{df_s} cases out of \eqn{N} total cases:
#'         \deqn{w_s = \log\left(\frac{N}{df_s}\right) + 1}
#'         Sections appearing in fewer cases receive higher weights.
#'   \item \bold{Cosine Similarity:} For cases \eqn{i} and \eqn{j} with weighted profiles \eqn{w_i} and \eqn{w_j}:
#'         \deqn{\text{sim}(i, j) = \frac{w_i \cdot w_j}{\|w_i\|_2 \|w_j\|_2}}
#'   \item \bold{k-NN Pruning & Deterministic Symmetrization:} Each case selects up to \eqn{k} neighbours with highest non-zero similarity (ties broken deterministically by case ID). An undirected edge is created if either case selects the other.
#' }
#'
#' @examples
#' syn_sec <- data.frame(
#'   case_id = c(1, 1, 2, 2, 3),
#'   act = c("NI Act", "NI Act", "NI Act", "NI Act", "IPC"),
#'   section = c("138", "141", "138", "142", "302")
#' )
#' sec_g <- build_section_graph(syn_sec)
#' diff_p <- diffuse_profiles(syn_sec, sec_g)
#' case_g <- build_case_graph(diff_p, k = 2)
#' print(case_g)
#'
#' @export
build_case_graph <- function(profiles,
                             k = 3,
                             use_idf = TRUE) {
  if (!inherits(profiles, "diffused_profiles") && !is.list(profiles)) {
    stop("`profiles` must be a `diffused_profiles` object.", call. = FALSE)
  }

  mat <- profiles$profiles
  case_ids <- profiles$case_ids
  sec_ids <- profiles$section_ids

  if (is.null(mat) || (!inherits(mat, "Matrix") && !is.matrix(mat))) {
    stop("`profiles` object must contain a valid `profiles` matrix.", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 1 || floor(k) != k) {
    stop("`k` must be a positive integer.", call. = FALSE)
  }

  if (length(case_ids) != nrow(mat)) {
    stop("Length of `case_ids` does not match the number of rows in `profiles` matrix.", call. = FALSE)
  }

  if (any(is.na(case_ids)) || any(nchar(trimws(as.character(case_ids))) == 0)) {
    stop("Missing or empty case IDs detected in `profiles`.", call. = FALSE)
  }

  if (any(duplicated(case_ids))) {
    stop("Duplicate case IDs detected in `profiles`.", call. = FALSE)
  }

  n_cases <- nrow(mat)
  m_sections <- ncol(mat)

  # Handle 0 cases or 0 sections
  if (n_cases == 0 || m_sections == 0) {
    g <- igraph::make_empty_graph(n = n_cases, directed = FALSE)
    if (n_cases > 0) {
      igraph::V(g)$name <- case_ids
      igraph::V(g)$case_id <- case_ids
    } else {
      g <- igraph::set_vertex_attr(g, "case_id", value = character(0))
      g <- igraph::set_vertex_attr(g, "name", value = character(0))
    }
    g <- igraph::set_edge_attr(g, "weight", value = numeric(0))
    return(g)
  }

  # Ensure matrix values are non-negative
  if (any(mat < 0)) {
    stop("`profiles` matrix contains negative values.", call. = FALSE)
  }

  # Step 1: IDF Rarity Weighting
  if (use_idf && n_cases > 0) {
    df_s <- Matrix::colSums(mat > 0)
    idf <- ifelse(df_s > 0, log(n_cases / df_s) + 1, 0)
    W <- mat %*% Matrix::Diagonal(x = idf)
  } else {
    W <- mat
  }

  # Step 2: Cosine Similarity with Chunking to maintain O(chunk_size * N) memory
  row_l2 <- sqrt(Matrix::rowSums(W^2))
  inv_l2 <- ifelse(row_l2 > 0, 1 / row_l2, 0)
  W_norm <- Matrix::Diagonal(x = inv_l2) %*% W

  W_norm_t <- Matrix::t(W_norm)

  chunk_size <- 1000L
  n_chunks <- ceiling(n_cases / chunk_size)

  case_id_ranks <- rank(case_ids, ties.method = "min")

  max_possible_edges <- n_cases * k
  edge_u <- integer(max_possible_edges)
  edge_v <- integer(max_possible_edges)
  edge_w <- numeric(max_possible_edges)
  ptr <- 0L

  for (ch in seq_len(n_chunks)) {
    start_idx <- (ch - 1L) * chunk_size + 1L
    end_idx <- min(ch * chunk_size, n_cases)
    chunk_rows <- start_idx:end_idx

    S_block <- as.matrix(W_norm[chunk_rows, , drop = FALSE] %*% W_norm_t)
    S_block[S_block < 0] <- 0
    S_block[S_block > 1] <- 1

    for (r in seq_along(chunk_rows)) {
      i <- chunk_rows[r]
      S_block[r, i] <- 0 # Zero out diagonal (no self-loops)

      sims <- S_block[r, ]
      cand_idx <- which(sims > 0)
      if (length(cand_idx) == 0) next

      cand_sims <- sims[cand_idx]
      cand_ranks <- case_id_ranks[cand_idx]

      ord <- order(-cand_sims, cand_ranks)
      selected_idx <- cand_idx[ord[seq_len(min(k, length(ord)))]]
      n_sel <- length(selected_idx)

      if (n_sel > 0) {
        slot_idx <- (ptr + 1L):(ptr + n_sel)
        edge_u[slot_idx] <- pmin(i, selected_idx)
        edge_v[slot_idx] <- pmax(i, selected_idx)
        edge_w[slot_idx] <- sims[selected_idx]
        ptr <- ptr + n_sel
      }
    }
  }

  if (ptr == 0L) {
    nodes_df <- data.frame(
      name = case_ids,
      case_id = case_ids,
      stringsAsFactors = FALSE
    )
    g <- igraph::graph_from_data_frame(
      d = data.frame(from = character(0), to = character(0), weight = numeric(0)),
      vertices = nodes_df,
      directed = FALSE
    )
    return(g)
  }

  edge_u <- edge_u[seq_len(ptr)]
  edge_v <- edge_v[seq_len(ptr)]
  edge_w <- edge_w[seq_len(ptr)]

  nodes_df <- data.frame(
    name = case_ids,
    case_id = case_ids,
    stringsAsFactors = FALSE
  )

  if (length(edge_u) == 0) {
    g <- igraph::graph_from_data_frame(
      d = data.frame(from = character(0), to = character(0), weight = numeric(0)),
      vertices = nodes_df,
      directed = FALSE
    )
    return(g)
  }

  pair_keys <- paste0(edge_u, "_", edge_v)
  dup_mask <- duplicated(pair_keys)

  edges_df <- data.frame(
    from = case_ids[edge_u[!dup_mask]],
    to = case_ids[edge_v[!dup_mask]],
    weight = edge_w[!dup_mask],
    stringsAsFactors = FALSE
  )

  edges_df <- edges_df[order(edges_df$from, edges_df$to), , drop = FALSE]
  rownames(edges_df) <- NULL

  g <- igraph::graph_from_data_frame(
    d = edges_df,
    vertices = nodes_df,
    directed = FALSE
  )

  g
}
