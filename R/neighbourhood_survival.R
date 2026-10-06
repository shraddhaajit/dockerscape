#' Censoring-Aware Neighbourhood Survival Curve Estimator
#'
#' Estimates a case-specific weighted Kaplan-Meier survival curve using the legally
#' similar neighbouring cases of a target case in the case similarity graph.
#'
#' @param case_graph An \code{igraph} graph of cases (e.g. produced by \code{\link{build_case_graph}}).
#' @param cases_clean A \code{data.frame} of cleaned cases (e.g. produced by \code{\link{clean_cases}}).
#' @param case_id Character string or numeric value specifying the target case ID.
#'
#' @return An S3 object of class \code{"neighbourhood_survival"} containing:
#' \describe{
#'   \item{target_case_id}{Character string of the target case ID.}
#'   \item{target_status}{Character status (\code{"pending"}, \code{"disposed"}, or \code{"invalid"}).}
#'   \item{target_duration}{Observed age/duration of the target case.}
#'   \item{target_event}{Event indicator of the target case.}
#'   \item{neighbours}{\code{data.frame} containing neighbour case IDs, weights, durations, and event indicators.}
#'   \item{n_neighbours}{Integer count of valid neighbours used.}
#'   \item{survival_table}{\code{data.frame} with columns \code{time}, \code{n_at_risk}, \code{n_events}, \code{surv}.}
#'   \item{median_survival}{Estimated median resolution time (days), or \code{NA_real_} if not reached.}
#' }
#'
#' @details
#' \bold{Weighted Kaplan-Meier Estimator:}
#' For a target case \eqn{i}, let \eqn{j \in \text{Neighbours}(i)} be its legally similar neighbours in \code{case_graph}
#' with similarity weights \eqn{w_j}.
#' Target case \eqn{i} is explicitly excluded from its own neighbourhood to prevent outcome leakage.
#'
#' At each distinct disposal event time \eqn{t_{(l)}}:
#' \deqn{Y(t_{(l)}) = \sum_{j: T_j \ge t_{(l)}} w_j \quad (\text{Weighted Risk Set})}
#' \deqn{d(t_{(l)}) = \sum_{j: T_j = t_{(l)}, \delta_j = 1} w_j \quad (\text{Weighted Disposals})}
#' \deqn{S(t) = \prod_{t_{(l)} \le t} \left(1 - \frac{d(t_{(l)})}{Y(t_{(l)})}\right)}
#'
#' Right-censored neighbours (\eqn{\delta_j = 0}) remain in the risk set \eqn{Y(t)} until their censoring duration
#' but do not contribute to event counts \eqn{d(t)}.
#'
#' @examples
#' cases <- data.frame(
#'   case_id = 1:3,
#'   filing_date = as.Date(c("2012-01-10", "2012-03-05", "2012-06-20")),
#'   decision_date = as.Date(c("2012-11-02", "2013-02-14", NA))
#' )
#' cleaned <- clean_cases(cases, observation_end = "2014-12-31")
#' sec <- data.frame(
#'   case_id = c(1, 1, 2, 2, 3),
#'   act = c("NI Act", "NI Act", "NI Act", "NI Act", "IPC"),
#'   section = c("138", "141", "138", "142", "302")
#' )
#' sg <- build_section_graph(sec)
#' dp <- diffuse_profiles(sec, sg)
#' cg <- build_case_graph(dp, k = 2)
#' km_res <- neighbourhood_survival(cg, cleaned, case_id = 1)
#' print(km_res)
#'
#' @export
neighbourhood_survival <- function(case_graph,
                                    cases_clean,
                                    case_id) {
  if (!inherits(case_graph, "igraph")) {
    stop("`case_graph` must be an igraph object.", call. = FALSE)
  }

  if (!is.data.frame(cases_clean)) {
    stop("`cases_clean` must be a data.frame.", call. = FALSE)
  }

  if (missing(case_id) || is.null(case_id) || is.na(case_id) || length(case_id) != 1) {
    stop("`case_id` must be a single valid case identifier.", call. = FALSE)
  }

  target_id <- as.character(case_id)

  # Check if target case exists in cases_clean
  if (!"case_id" %in% names(cases_clean)) {
    stop("Column `case_id` not found in `cases_clean`.", call. = FALSE)
  }

  target_clean_idx <- which(as.character(cases_clean$case_id) == target_id)
  if (length(target_clean_idx) == 0) {
    stop(sprintf("Target case_id '%s' not found in `cases_clean`.", target_id), call. = FALSE)
  }

  target_row <- cases_clean[target_clean_idx[1], ]
  t_duration <- if ("duration" %in% names(target_row)) target_row$duration else NA_real_
  t_event <- if ("event" %in% names(target_row)) target_row$event else NA_real_
  t_status <- if (is.na(t_event)) "invalid" else if (t_event == 1) "disposed" else "pending"

  # Find neighbours in case_graph
  graph_node_names <- igraph::V(case_graph)$name
  if (is.null(graph_node_names) || !target_id %in% graph_node_names) {
    # Target case not in graph or graph empty
    res <- list(
      target_case_id = target_id,
      target_status = t_status,
      target_duration = t_duration,
      target_event = t_event,
      neighbours = data.frame(case_id = character(0), weight = numeric(0), duration = numeric(0), event = numeric(0)),
      n_neighbours = 0,
      survival_table = data.frame(time = 0, n_at_risk = 0, n_events = 0, surv = 1.0),
      median_survival = NA_real_
    )
    class(res) <- "neighbourhood_survival"
    return(res)
  }

  # Extract incident edges for target_id
  adj_edges <- igraph::incident(case_graph, v = target_id, mode = "all")

  if (length(adj_edges) == 0) {
    # Isolated node in case graph
    res <- list(
      target_case_id = target_id,
      target_status = t_status,
      target_duration = t_duration,
      target_event = t_event,
      neighbours = data.frame(case_id = character(0), weight = numeric(0), duration = numeric(0), event = numeric(0)),
      n_neighbours = 0,
      survival_table = data.frame(time = 0, n_at_risk = 0, n_events = 0, surv = 1.0),
      median_survival = NA_real_
    )
    class(res) <- "neighbourhood_survival"
    return(res)
  }

  # Get neighbour IDs and weights
  edge_weights <- igraph::E(case_graph)$weight[adj_edges]
  ends_mat <- igraph::ends(case_graph, adj_edges)

  neigh_ids <- ifelse(ends_mat[, 1] == target_id, ends_mat[, 2], ends_mat[, 1])

  # Match neighbours against cases_clean
  clean_cases_map <- data.frame(
    case_id = as.character(cases_clean$case_id),
    duration = cases_clean$duration,
    event = cases_clean$event,
    is_valid = if ("is_valid" %in% names(cases_clean)) cases_clean$is_valid else TRUE,
    stringsAsFactors = FALSE
  )

  # Target exclusion explicitly enforced (filter out any self-loop if present)
  valid_neigh_mask <- (neigh_ids != target_id)
  neigh_ids <- neigh_ids[valid_neigh_mask]
  edge_weights <- edge_weights[valid_neigh_mask]

  if (length(neigh_ids) == 0) {
    res <- list(
      target_case_id = target_id,
      target_status = t_status,
      target_duration = t_duration,
      target_event = t_event,
      neighbours = data.frame(case_id = character(0), weight = numeric(0), duration = numeric(0), event = numeric(0)),
      n_neighbours = 0,
      survival_table = data.frame(time = 0, n_at_risk = 0, n_events = 0, surv = 1.0),
      median_survival = NA_real_
    )
    class(res) <- "neighbourhood_survival"
    return(res)
  }

  # Join neighbour IDs with cases_clean
  neigh_df <- data.frame(
    case_id = neigh_ids,
    weight = edge_weights,
    stringsAsFactors = FALSE
  )

  merged_neigh <- merge(neigh_df, clean_cases_map, by = "case_id", all.x = TRUE)

  # Filter out missing or invalid neighbour records
  merged_neigh <- merged_neigh[
    !is.na(merged_neigh$duration) &
    !is.na(merged_neigh$event) &
    !is.na(merged_neigh$weight) &
    merged_neigh$weight > 0 &
    merged_neigh$is_valid, , drop = FALSE
  ]

  if (nrow(merged_neigh) == 0) {
    res <- list(
      target_case_id = target_id,
      target_status = t_status,
      target_duration = t_duration,
      target_event = t_event,
      neighbours = data.frame(case_id = character(0), weight = numeric(0), duration = numeric(0), event = numeric(0)),
      n_neighbours = 0,
      survival_table = data.frame(time = 0, n_at_risk = 0, n_events = 0, surv = 1.0),
      median_survival = NA_real_
    )
    class(res) <- "neighbourhood_survival"
    return(res)
  }

  # Order neighbours deterministically by duration, event descending, case_id
  merged_neigh <- merged_neigh[order(merged_neigh$duration, -merged_neigh$event, merged_neigh$case_id), , drop = FALSE]
  rownames(merged_neigh) <- NULL

  # Compute Weighted Kaplan-Meier Curve
  event_times <- sort(unique(merged_neigh$duration[merged_neigh$event == 1]))

  # Time 0 initialization
  times_vec <- c(0)
  risk_vec <- c(sum(merged_neigh$weight))
  events_vec <- c(0)
  surv_vec <- c(1.0)

  current_surv <- 1.0

  if (length(event_times) > 0) {
    for (t_ev in event_times) {
      # Risk set Y(t): neighbours with duration >= t_ev
      at_risk_idx <- which(merged_neigh$duration >= t_ev)
      y_risk <- sum(merged_neigh$weight[at_risk_idx])

      # Events d(t): neighbours with duration == t_ev and event == 1
      event_idx <- which(merged_neigh$duration == t_ev & merged_neigh$event == 1)
      d_events <- sum(merged_neigh$weight[event_idx])

      if (y_risk > 0) {
        step_val <- 1 - (d_events / y_risk)
        current_surv <- current_surv * step_val
      }

      times_vec <- c(times_vec, t_ev)
      risk_vec <- c(risk_vec, y_risk)
      events_vec <- c(events_vec, d_events)
      surv_vec <- c(surv_vec, current_surv)
    }
  }

  surv_table <- data.frame(
    time = times_vec,
    n_at_risk = risk_vec,
    n_events = events_vec,
    surv = surv_vec,
    stringsAsFactors = FALSE
  )

  # Compute Median Survival Time (interpolated or NA)
  med_idx <- which(surv_table$surv <= 0.5)
  if (length(med_idx) > 0) {
    median_survival <- surv_table$time[med_idx[1]]
  } else {
    median_survival <- NA_real_
  }

  res <- list(
    target_case_id = target_id,
    target_status = t_status,
    target_duration = t_duration,
    target_event = t_event,
    neighbours = merged_neigh[, c("case_id", "weight", "duration", "event")],
    n_neighbours = nrow(merged_neigh),
    survival_table = surv_table,
    median_survival = median_survival
  )
  class(res) <- "neighbourhood_survival"
  res
}

#' Print method for neighbourhood_survival
#' @param x A \code{neighbourhood_survival} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print neighbourhood_survival
print.neighbourhood_survival <- function(x, ...) {
  cat("Neighbourhood Survival Estimate\n")
  cat("===============================\n")
  cat("Target Case ID:   ", x$target_case_id, "\n")
  cat("Target Status:    ", x$target_status, "\n")
  if (!is.na(x$target_duration)) {
    cat("Target Duration:  ", x$target_duration, "days\n")
  }
  cat("Valid Neighbours: ", x$n_neighbours, "\n")
  if (!is.na(x$median_survival)) {
    cat("Median Survival:  ", x$median_survival, "days\n")
  } else {
    cat("Median Survival:   Not reached (> ", max(x$survival_table$time), " days)\n", sep = "")
  }
  cat("\nEstimated Survival Table:\n")
  print(x$survival_table)
  invisible(x)
}
