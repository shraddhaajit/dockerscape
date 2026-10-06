#' Audit Future-Neighbour Leakage in Case Similarity Graph
#'
#' Identifies graph edges where a case is connected in \code{case_graph} to a case filed later
#' than a specified temporal cutoff date (or later than its own filing date).
#'
#' @param case_graph An \code{igraph} graph object produced by \code{\link{build_case_graph}}.
#' @param cases_clean A \code{data.frame} of cleaned cases containing \code{case_id} and \code{filing_date}.
#' @param cutoff_date Optional \code{Date} threshold separating training cases from validation cases.
#'
#' @return An S3 object of class \code{c("future_neighbour_audit", "list")} containing:
#' \describe{
#'   \item{available}{Logical indicating if the audit was evaluated successfully.}
#'   \item{cutoff_date}{\code{Date} threshold used, or \code{NULL} if auditing full graph temporal order.}
#'   \item{n_edges_checked}{Total number of edges in \code{case_graph}.}
#'   \item{n_future_neighbour_edges}{Count of future-neighbour leakage edges.}
#'   \item{n_cases_with_future_neighbours}{Count of distinct cases with at least one future neighbour.}
#'   \item{affected_case_ids}{Character vector of affected case IDs.}
#'   \item{future_neighbour_edges}{A \code{data.frame} of individual leakage edges.}
#'   \item{max_future_neighbour_filing_date}{Latest filing date among identified future neighbours.}
#'   \item{interpretation}{Statistical interpretation of audit findings.}
#' }
#'
#' @details
#' \bold{Future-Neighbour Directionality:}
#' For an undirected edge between Case A (filed 2012) and Case B (filed 2013), Case A has a future neighbour
#' (Case B), whereas Case B has a past neighbour (Case A).
#' If \code{cutoff_date} is provided, only edges where a pre-cutoff case is connected to a post-cutoff case are flagged.
#'
#' \bold{Statistical Interpretation:}
#' Future-neighbour leakage indicates that a case's legal similarity profile was constructed using cases filed after
#' its temporal cutoff. This is an audit of information leakage, not proof of an invalid model.
#' @examples
#' cases <- data.frame(
#'   case_id = c("1", "2"),
#'   filing_date = as.Date(c("2012-01-10", "2013-05-15"))
#' )
#' g <- igraph::make_graph(c("1", "2"), directed = FALSE)
#' audit <- check_future_neighbours(g, cases, cutoff_date = as.Date("2012-12-31"))
#' print(audit)
#'
#' @export
check_future_neighbours <- function(case_graph,
                                    cases_clean,
                                    cutoff_date = NULL) {
  if (!inherits(case_graph, "igraph")) {
    stop("`case_graph` must be an igraph object.", call. = FALSE)
  }
  if (!is.data.frame(cases_clean) || !all(c("case_id", "filing_date") %in% names(cases_clean))) {
    stop("`cases_clean` must be a data.frame containing 'case_id' and 'filing_date'.", call. = FALSE)
  }

  n_edges <- igraph::ecount(case_graph)
  if (n_edges == 0) {
    res <- list(
      available = TRUE,
      cutoff_date = cutoff_date,
      n_edges_checked = 0L,
      n_future_neighbour_edges = 0L,
      n_cases_with_future_neighbours = 0L,
      affected_case_ids = character(0),
      future_neighbour_edges = data.frame(
        case_id = character(0),
        neighbour_id = character(0),
        case_filing_date = as.Date(character(0)),
        neighbour_filing_date = as.Date(character(0)),
        edge_weight = numeric(0),
        temporal_gap_days = integer(0),
        stringsAsFactors = FALSE
      ),
      max_future_neighbour_filing_date = as.Date(NA),
      interpretation = "No edges present in case_graph to audit."
    )
    class(res) <- c("future_neighbour_audit", "list")
    return(res)
  }

  el <- igraph::as_data_frame(case_graph, what = "edges")
  if (!"weight" %in% names(el)) {
    el$weight <- 1.0
  }

  f_map <- stats::setNames(as.Date(cases_clean$filing_date), as.character(cases_clean$case_id))

  leak_rows <- list()
  idx <- 1

  cutoff <- if (!is.null(cutoff_date)) as.Date(cutoff_date) else NULL

  for (k in seq_len(nrow(el))) {
    u <- as.character(el$from[k])
    v <- as.character(el$to[k])
    w <- el$weight[k]

    d_u <- f_map[u]
    d_v <- f_map[v]

    if (is.na(d_u) || is.na(d_v)) next

    # Check u -> v
    if (!is.null(cutoff)) {
      if (d_u <= cutoff && d_v > cutoff) {
        leak_rows[[idx]] <- data.frame(
          case_id = u,
          neighbour_id = v,
          case_filing_date = d_u,
          neighbour_filing_date = d_v,
          edge_weight = w,
          temporal_gap_days = as.integer(d_v - d_u),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
      if (d_v <= cutoff && d_u > cutoff) {
        leak_rows[[idx]] <- data.frame(
          case_id = v,
          neighbour_id = u,
          case_filing_date = d_v,
          neighbour_filing_date = d_u,
          edge_weight = w,
          temporal_gap_days = as.integer(d_u - d_v),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    } else {
      if (d_v > d_u) {
        leak_rows[[idx]] <- data.frame(
          case_id = u,
          neighbour_id = v,
          case_filing_date = d_u,
          neighbour_filing_date = d_v,
          edge_weight = w,
          temporal_gap_days = as.integer(d_v - d_u),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      } else if (d_u > d_v) {
        leak_rows[[idx]] <- data.frame(
          case_id = v,
          neighbour_id = u,
          case_filing_date = d_v,
          neighbour_filing_date = d_u,
          edge_weight = w,
          temporal_gap_days = as.integer(d_u - d_v),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }

  if (length(leak_rows) == 0) {
    leak_df <- data.frame(
      case_id = character(0),
      neighbour_id = character(0),
      case_filing_date = as.Date(character(0)),
      neighbour_filing_date = as.Date(character(0)),
      edge_weight = numeric(0),
      temporal_gap_days = integer(0),
      stringsAsFactors = FALSE
    )
    res <- list(
      available = TRUE,
      cutoff_date = cutoff,
      n_edges_checked = n_edges,
      n_future_neighbour_edges = 0L,
      n_cases_with_future_neighbours = 0L,
      affected_case_ids = character(0),
      future_neighbour_edges = leak_df,
      max_future_neighbour_filing_date = as.Date(NA),
      interpretation = "No future-neighbour leakage edges detected."
    )
    class(res) <- c("future_neighbour_audit", "list")
    return(res)
  }

  leak_df <- do.call(rbind, leak_rows)
  leak_df <- leak_df[order(leak_df$case_id, leak_df$neighbour_filing_date), , drop = FALSE]
  rownames(leak_df) <- NULL

  aff_cases <- sort(unique(leak_df$case_id))
  max_fut_date <- max(leak_df$neighbour_filing_date, na.rm = TRUE)

  interp_str <- sprintf(
    "Identified %d future-neighbour leakage edges across %d cases. Legal similarity scores for these cases incorporate edges to cases filed after their relevant cutoff date.",
    nrow(leak_df), length(aff_cases)
  )

  res <- list(
    available = TRUE,
    cutoff_date = cutoff,
    n_edges_checked = n_edges,
    n_future_neighbour_edges = nrow(leak_df),
    n_cases_with_future_neighbours = length(aff_cases),
    affected_case_ids = aff_cases,
    future_neighbour_edges = leak_df,
    max_future_neighbour_filing_date = max_fut_date,
    interpretation = interp_str
  )
  class(res) <- c("future_neighbour_audit", "list")
  res
}

#' Print method for future_neighbour_audit
#' @param x A \code{future_neighbour_audit} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print future_neighbour_audit
print.future_neighbour_audit <- function(x, ...) {
  cat("Future-Neighbour Graph Leakage Audit\n")
  cat("===================================\n")
  cat("Cutoff Date:                       ", if(!is.null(x$cutoff_date)) as.character(x$cutoff_date) else "None (Full Graph Ordering)", "\n")
  cat("Edges Checked:                     ", x$n_edges_checked, "\n")
  cat("Future-Neighbour Edges Detected:   ", x$n_future_neighbour_edges, "\n")
  cat("Cases with Future Neighbours:      ", x$n_cases_with_future_neighbours, "\n")
  if (x$n_future_neighbour_edges > 0) {
    cat("Latest Future-Neighbour Filing:    ", as.character(x$max_future_neighbour_filing_date), "\n\n")
    cat("Head of Future-Neighbour Edge Table:\n")
    print(head(x$future_neighbour_edges, 6))
    cat("\n")
  } else {
    cat("\n")
  }
  cat("Statistical Interpretation:\n")
  cat(x$interpretation, "\n")
  invisible(x)
}

#' Audit Court Representation Overlap Across Temporal Split
#'
#' Evaluates court representation overlap between training and validation case sets.
#'
#' @param train_cases A \code{data.frame} or vector of court identifiers for the training set.
#' @param validation_cases A \code{data.frame} or vector of court identifiers for the validation set.
#' @param court Character string specifying the court column name if data frames are provided. Defaults to \code{"court"}.
#'
#' @return An S3 object of class \code{c("court_overlap_audit", "list")} containing:
#' \describe{
#'   \item{training_courts}{Sorted vector of unique training courts.}
#'   \item{validation_courts}{Sorted vector of unique validation courts.}
#'   \item{overlapping_courts}{Sorted vector of shared courts.}
#'   \item{n_overlapping_courts}{Count of shared courts.}
#'   \item{training_only_courts}{Vector of courts present only in training.}
#'   \item{validation_only_courts}{Vector of courts present only in validation.}
#'   \item{overlap_ratio}{Fraction of validation courts present in training.}
#'   \item{interpretation}{Statistical interpretation of court overlap.}
#' }
#'
#' @details
#' Court overlap describes whether validation tests generalisation across time within known courts
#' or tests generalisation to entirely unseen courts. Shared courts are a validation design feature, not data leakage.
#'
#' @examples
#' train_df <- data.frame(court = c("C1", "C2"))
#' val_df <- data.frame(court = c("C1", "C3"))
#' audit <- check_court_overlap(train_df, val_df)
#' print(audit)
#'
#' @export
check_court_overlap <- function(train_cases,
                                validation_cases,
                                court = "court") {
  get_courts <- function(obj) {
    if (is.data.frame(obj)) {
      if (!court %in% names(obj)) {
        stop(sprintf("Column '%s' not found in data frame.", court), call. = FALSE)
      }
      vals <- as.character(obj[[court]])
    } else {
      vals <- as.character(obj)
    }
    vals <- trimws(vals[!is.na(vals) & nchar(trimws(as.character(vals))) > 0])
    sort(unique(vals))
  }

  c_train <- get_courts(train_cases)
  c_val <- get_courts(validation_cases)

  overlap <- sort(intersect(c_train, c_val))
  tr_only <- sort(setdiff(c_train, c_val))
  val_only <- sort(setdiff(c_val, c_train))

  overlap_ratio <- if (length(c_val) > 0) length(overlap) / length(c_val) else NA_real_

  interp_str <- sprintf(
    "Court overlap audit found %d shared courts out of %d validation courts (overlap ratio: %.1f%%). Court overlap describes whether validation tests temporal generalisation within known courts or across unseen courts; it is not data leakage.",
    length(overlap), length(c_val), ifelse(is.na(overlap_ratio), 0, overlap_ratio * 100)
  )

  res <- list(
    training_courts = c_train,
    validation_courts = c_val,
    overlapping_courts = overlap,
    n_overlapping_courts = length(overlap),
    training_only_courts = tr_only,
    validation_only_courts = val_only,
    overlap_ratio = overlap_ratio,
    interpretation = interp_str
  )
  class(res) <- c("court_overlap_audit", "list")
  res
}

#' Print method for court_overlap_audit
#' @param x A \code{court_overlap_audit} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print court_overlap_audit
print.court_overlap_audit <- function(x, ...) {
  cat("Court Representation Overlap Audit\n")
  cat("=================================\n")
  cat("Training Courts:        ", length(x$training_courts), " (", paste(head(x$training_courts, 5), collapse = ", "), if(length(x$training_courts)>5)"...", ")\n", sep = "")
  cat("Validation Courts:      ", length(x$validation_courts), " (", paste(head(x$validation_courts, 5), collapse = ", "), if(length(x$validation_courts)>5)"...", ")\n", sep = "")
  cat("Overlapping Courts:     ", x$n_overlapping_courts, " (", paste(head(x$overlapping_courts, 5), collapse = ", "), if(length(x$overlapping_courts)>5)"...", ")\n", sep = "")
  cat("Validation-Only Courts: ", length(x$validation_only_courts), if(length(x$validation_only_courts)>0) paste(" (", paste(head(x$validation_only_courts, 5), collapse = ", "), ")", sep="") else "", "\n\n")

  cat("Statistical Interpretation:\n")
  cat(x$interpretation, "\n")
  invisible(x)
}

#' Audit Same-Date/Court Clusters and Exact Duplicate Records
#'
#' Identifies same-date/court case clusters and exact duplicate rows in cleaned case data based on court, filing date, and decision date.
#'
#' @param cases_clean A \code{data.frame} of cleaned case records.
#' @param court Character string specifying the court column name in \code{cases_clean}. Defaults to \code{"court"}.
#'
#' @return An S3 object of class \code{c("near_duplicate_audit", "list")} containing:
#' \describe{
#'   \item{available}{Logical indicating if audit evaluated successfully.}
#'   \item{n_rows_checked}{Total number of case rows audited.}
#'   \item{n_same_date_court_groups}{Number of distinct same-date/court clusters.}
#'   \item{n_rows_in_duplicate_groups}{Total number of case rows belonging to clusters.}
#'   \item{duplicate_groups}{A data frame detailing clusters and case IDs.}
#'   \item{interpretation}{Statistical interpretation of audit findings.}
#' }
#'
#' @details
#' Audit checks for multiple case records sharing identical court, filing date, and decision date.
#' Cases with distinct \code{case_id}s sharing dates/courts form same-date/court clusters, which are explicitly distinguished
#' from duplicate case records. Fuzzy near-duplicate matching on unstructured text is not performed as case-level text is not present.
#'
#' @examples
#' cases <- data.frame(
#'   case_id = 1:3,
#'   court = c("C1", "C1", "C2"),
#'   filing_date = as.Date(c("2012-01-10", "2012-01-10", "2012-03-05")),
#'   decision_date = as.Date(c("2012-11-02", "2012-11-02", "2013-02-14"))
#' )
#' audit <- check_near_duplicates(cases)
#' print(audit)
#'
#' @export
check_near_duplicates <- function(cases_clean, court = "court") {
  if (!is.data.frame(cases_clean)) {
    stop("`cases_clean` must be a data.frame.", call. = FALSE)
  }

  n_total <- nrow(cases_clean)
  if (n_total == 0) {
    res <- list(
      available = TRUE,
      n_rows_checked = 0L,
      n_same_date_court_groups = 0L,
      n_exact_duplicate_groups = 0L,
      n_rows_in_duplicate_groups = 0L,
      duplicate_groups = data.frame(),
      interpretation = "Zero case rows audited."
    )
    class(res) <- c("near_duplicate_audit", "list")
    return(res)
  }

  c_col <- if (court %in% names(cases_clean)) court else "court"
  req_cols <- c("case_id", "filing_date")
  if (!all(req_cols %in% names(cases_clean))) {
    stop("`cases_clean` must contain 'case_id' and 'filing_date'.", call. = FALSE)
  }

  dec_str <- if ("decision_date" %in% names(cases_clean)) as.character(cases_clean$decision_date) else rep("NA", n_total)
  court_str <- if (c_col %in% names(cases_clean)) as.character(cases_clean[[c_col]]) else rep("NA", n_total)
  filing_str <- as.character(cases_clean$filing_date)

  match_key <- paste(court_str, filing_str, dec_str, sep = "||")

  dup_counts <- table(match_key)
  dup_keys <- names(dup_counts[dup_counts > 1])

  if (length(dup_keys) == 0) {
    res <- list(
      available = TRUE,
      n_rows_checked = n_total,
      n_same_date_court_groups = 0L,
      n_exact_duplicate_groups = 0L,
      n_rows_in_duplicate_groups = 0L,
      duplicate_groups = data.frame(
        group_id = integer(0),
        court = character(0),
        filing_date = as.Date(character(0)),
        decision_date = as.Date(character(0)),
        group_size = integer(0),
        case_ids = character(0),
        stringsAsFactors = FALSE
      ),
      interpretation = "Audit checked exact match keys (court, filing_date, decision_date); no same-date/court clusters were found."
    )
    class(res) <- c("near_duplicate_audit", "list")
    return(res)
  }

  grp_rows <- list()
  idx <- 1
  total_dup_rows <- 0L

  for (key in dup_keys) {
    matching_indices <- which(match_key == key)
    c_ids <- paste(cases_clean$case_id[matching_indices], collapse = ", ")
    c_val <- court_str[matching_indices[1]]
    f_val <- as.Date(filing_str[matching_indices[1]])
    d_val <- if ("decision_date" %in% names(cases_clean)) as.Date(cases_clean$decision_date[matching_indices[1]]) else as.Date(NA)
    g_size <- length(matching_indices)
    total_dup_rows <- total_dup_rows + g_size

    grp_rows[[idx]] <- data.frame(
      group_id = idx,
      court = c_val,
      filing_date = f_val,
      decision_date = d_val,
      group_size = g_size,
      case_ids = c_ids,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }

  grp_df <- do.call(rbind, grp_rows)
  grp_df <- grp_df[order(-grp_df$group_size), , drop = FALSE]
  rownames(grp_df) <- NULL

  interp_str <- sprintf(
    "Audit checked exact match keys (court, filing_date, decision_date) across %d rows and identified %d same-date/court clusters containing %d total rows. Same-date/court clusters contain distinct case_ids and are distinguished from duplicate case records. Fuzzy near-duplicate matching is not performed as unstructured case text is not present.",
    n_total, length(dup_keys), total_dup_rows
  )

  res <- list(
    available = TRUE,
    n_rows_checked = n_total,
    n_same_date_court_groups = length(dup_keys),
    n_exact_duplicate_groups = length(dup_keys),
    n_rows_in_duplicate_groups = total_dup_rows,
    duplicate_groups = grp_df,
    interpretation = interp_str
  )
  class(res) <- c("near_duplicate_audit", "list")
  res
}

#' Print method for near_duplicate_audit
#' @param x A \code{near_duplicate_audit} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print near_duplicate_audit
print.near_duplicate_audit <- function(x, ...) {
  cat("Same-Date/Court Cluster & Duplicate Audit\n")
  cat("==========================================\n")
  cat("Rows Audited:                ", x$n_rows_checked, "\n")
  cat("Same-Date/Court Clusters:    ", x$n_same_date_court_groups, "\n")
  cat("Rows in Clusters:            ", x$n_rows_in_duplicate_groups, "\n\n")

  if (x$n_same_date_court_groups > 0) {
    cat("Top Clusters:\n")
    print(head(x$duplicate_groups, 5))
    cat("\n")
  }

  cat("Statistical Interpretation:\n")
  cat(x$interpretation, "\n")
  invisible(x)
}
