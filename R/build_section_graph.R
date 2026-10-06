#' Construct a Legal Section Co-occurrence Graph
#'
#' Builds a weighted, undirected \code{igraph} graph where vertices represent unique
#' legal section identifiers (combination of Act and Section) and edge weights
#' represent the count of distinct cases in which two legal sections co-occur.
#'
#' @param sections A \code{data.frame} containing legal section records for cases.
#' @param case Character string specifying the column name for case IDs. Defaults to \code{"case_id"}.
#' @param act Character string specifying the column name for Act names. Defaults to \code{"act"}.
#' @param section Character string specifying the column name for Section numbers/names. Defaults to \code{"section"}.
#'
#' @return An undirected \code{igraph} graph object containing:
#' \describe{
#'   \item{Vertex attributes}{\code{name} / \code{identifier} (unique \code{"Act::Section"}), \code{act}, \code{section}.}
#'   \item{Edge attributes}{\code{weight} (integer count of distinct cases with co-occurrence).}
#' }
#'
#' @details
#' \bold{Graph Construction Details:}
#' \itemize{
#'   \item \bold{Identifiers:} Legal sections are identified by combining Act and Section (e.g., \code{"NI Act::138"}). This ensures sections from different Acts with identical section numbers remain distinct vertices.
#'   \item \bold{Deduplication:} Duplicate occurrences of the same \code{(case_id, act, section)} triplet are removed prior to counting to prevent artificial inflation of edge weights.
#'   \item \bold{Singletons:} Sections that appear without co-occurring with any other section in the same case are retained as isolated vertices with degree 0.
#'   \item \bold{Missing Data:} Rows containing \code{NA}, empty strings, or blank values in \code{case_id}, \code{act}, or \code{section} are excluded from graph construction.
#' }
#'
#' @examples
#' sections <- data.frame(
#'   case_id = c(1, 1, 2, 2, 3),
#'   act = c("NI Act", "NI Act", "NI Act", "NI Act", "IPC"),
#'   section = c("138", "141", "138", "142", "302")
#' )
#' g <- build_section_graph(sections)
#' print(g)
#'
#' @export
build_section_graph <- function(sections,
                                case = "case_id",
                                act = "act",
                                section = "section") {
  if (!inherits(sections, "data.frame")) {
    stop("`sections` must be a data.frame.", call. = FALSE)
  }

  req_cols <- c(case, act, section)
  missing_cols <- setdiff(req_cols, names(sections))
  if (length(missing_cols) > 0) {
    stop(sprintf("Column(s) not found in `sections`: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  case_vals <- sections[[case]]
  act_vals <- as.character(sections[[act]])
  sec_vals <- as.character(sections[[section]])

  # Filter out missing / NA / empty records
  valid_mask <- !is.na(case_vals) &
                !is.na(act_vals) & nchar(trimws(act_vals)) > 0 &
                !is.na(sec_vals) & nchar(trimws(sec_vals)) > 0

  if (!any(valid_mask)) {
    g <- igraph::make_empty_graph(n = 0, directed = FALSE)
    g <- igraph::set_vertex_attr(g, "act", value = character(0))
    g <- igraph::set_vertex_attr(g, "section", value = character(0))
    g <- igraph::set_vertex_attr(g, "identifier", value = character(0))
    return(g)
  }

  clean_df <- data.frame(
    case_id = case_vals[valid_mask],
    act = trimws(act_vals[valid_mask]),
    section = trimws(sec_vals[valid_mask]),
    stringsAsFactors = FALSE
  )

  clean_df$identifier <- paste0(clean_df$act, "::", clean_df$section)

  # Extract unique nodes deterministically
  nodes_df <- unique(clean_df[, c("identifier", "act", "section")])
  nodes_df <- nodes_df[order(nodes_df$identifier), , drop = FALSE]
  rownames(nodes_df) <- NULL

  # Deduplicate per case to prevent artificial inflation
  unique_case_sections <- unique(clean_df[, c("case_id", "identifier")])

  # Compute co-occurrences across cases
  case_list <- split(unique_case_sections$identifier, unique_case_sections$case_id)

  edge_pairs <- list()
  idx <- 1

  for (c_ids in case_list) {
    u_sec <- unique(c_ids)
    n_sec <- length(u_sec)
    if (n_sec >= 2) {
      pairs <- utils::combn(u_sec, 2, simplify = FALSE)
      for (p in pairs) {
        p_sorted <- sort(p)
        edge_pairs[[idx]] <- p_sorted
        idx <- idx + 1
      }
    }
  }

  if (length(edge_pairs) == 0) {
    g <- igraph::graph_from_data_frame(
      d = data.frame(from = character(0), to = character(0), weight = numeric(0)),
      vertices = nodes_df,
      directed = FALSE
    )
    igraph::V(g)$identifier <- igraph::V(g)$name
    return(g)
  }

  edges_raw <- do.call(rbind, lapply(edge_pairs, function(x) data.frame(from = x[1], to = x[2], stringsAsFactors = FALSE)))

  edge_counts <- stats::aggregate(
    list(weight = rep(1, nrow(edges_raw))),
    by = list(from = edges_raw$from, to = edges_raw$to),
    FUN = sum
  )

  edge_counts <- edge_counts[order(edge_counts$from, edge_counts$to), , drop = FALSE]
  rownames(edge_counts) <- NULL

  g <- igraph::graph_from_data_frame(
    d = edge_counts,
    vertices = nodes_df,
    directed = FALSE
  )

  igraph::V(g)$identifier <- igraph::V(g)$name

  g
}
