#' Compute Relative Lateness of a Case
#'
#' Evaluates a target case's current age or resolution duration relative to the
#' censoring-aware survival timeline of its legally similar neighbouring cases.
#'
#' @param neighbourhood An object of class \code{\link{neighbourhood_survival}} produced by \code{neighbourhood_survival()}.
#'
#' @return An S3 object of class \code{"relative_lateness"} containing:
#' \describe{
#'   \item{target_case_id}{Character string of the target case ID.}
#'   \item{target_status}{Character status (\code{"pending"} or \code{"disposed"}).}
#'   \item{target_duration}{Observed duration/age of the target case (days).}
#'   \item{survival_at_duration}{Survival probability \eqn{S_i(T_i)} from the neighbourhood curve at duration \eqn{T_i}, or \code{NA_real_} if not estimable.}
#'   \item{relative_position}{Relative position percentile \eqn{1 - S_i(T_i)} in \code{[0, 1]}, or \code{NA_real_} if not estimable.}
#'   \item{estimable}{Logical flag indicating whether the relative position is statistically estimable (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{interpretation}{Human-readable non-causal summary interpretation.}
#'   \item{n_neighbours}{Count of valid neighbouring cases used.}
#'   \item{median_neighbourhood_survival}{Median resolution time of neighbours (days), or \code{NA_real_}.}
#'   \item{neighbourhood}{The input \code{\link{neighbourhood_survival}} object.}
#' }
#'
#' @details
#' \bold{Estimability Rules:}
#' Relative position is considered \bold{\code{estimable = TRUE}} if either:
#' \enumerate{
#'   \item The survival curve has reached \code{0.0} at or before the target duration \eqn{T_i}.
#'   \item At least one resolution event (\eqn{\delta = 1}) occurred in the neighbourhood AND the target duration \eqn{T_i} does not exceed the maximum follow-up duration of the available neighbours.
#' }
#' Otherwise, \bold{\code{estimable = FALSE}} and \code{relative_position} is set to \code{NA_real_} (e.g. when all neighbours are censored, or when the target duration exceeds the maximum follow-up of all neighbours).
#'
#' \bold{Step-Function Convention:}
#' When estimable, \eqn{S_i(T_i)} is evaluated using Kaplan-Meier right-continuous step function logic:
#' \itemize{
#'   \item If \eqn{T_i} is before the first event time: \eqn{S_i(T_i) = 1.0}.
#'   \item If \eqn{T_i} lies between event times: \eqn{S_i(T_i)} equals the survival probability of the most recent event time \eqn{\le T_i}.
#' }
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
#' lateness <- relative_lateness(km_res)
#' print(lateness)
#'
#' @export
relative_lateness <- function(neighbourhood) {
  if (missing(neighbourhood) || is.null(neighbourhood)) {
    stop("`neighbourhood` must be provided.", call. = FALSE)
  }

  if (!inherits(neighbourhood, "neighbourhood_survival") && !is.list(neighbourhood)) {
    stop("`neighbourhood` must be a `neighbourhood_survival` object.", call. = FALSE)
  }

  target_id <- neighbourhood$target_case_id
  target_status <- neighbourhood$target_status
  t_duration <- neighbourhood$target_duration
  n_neigh <- neighbourhood$n_neighbours
  med_surv <- neighbourhood$median_survival
  stab <- neighbourhood$survival_table
  neigh_df <- neighbourhood$neighbours

  if (is.null(target_id)) {
    stop("`neighbourhood` object is missing `target_case_id`.", call. = FALSE)
  }

  if (is.null(t_duration) || is.na(t_duration)) {
    stop("`neighbourhood` object is missing a valid `target_duration`.", call. = FALSE)
  }

  if (!is.numeric(t_duration) || t_duration < 0) {
    stop("`target_duration` must be a non-negative number.", call. = FALSE)
  }

  # Handle case with 0 neighbours
  if (is.null(n_neigh) || n_neigh == 0 || is.null(neigh_df) || nrow(neigh_df) == 0) {
    res <- list(
      target_case_id = as.character(target_id),
      target_status = as.character(target_status),
      target_duration = t_duration,
      survival_at_duration = NA_real_,
      relative_position = NA_real_,
      estimable = FALSE,
      interpretation = "Relative position is not estimable because no valid neighbouring cases exist in the case graph.",
      n_neighbours = 0,
      median_neighbourhood_survival = NA_real_,
      neighbourhood = neighbourhood
    )
    class(res) <- "relative_lateness"
    return(res)
  }

  # Ensure survival_table is sorted by time
  stab <- stab[order(stab$time), , drop = FALSE]

  # Extract neighbour metrics for estimability evaluation
  max_followup <- max(neigh_df$duration, na.rm = TRUE)
  n_events <- sum(neigh_df$event == 1, na.rm = TRUE)

  # Step-function lookup: S(T_i) is survival probability of last event time <= T_i
  eligible_rows <- which(stab$time <= t_duration)

  if (length(eligible_rows) == 0) {
    # T_i is strictly before first time in table (t = 0)
    surv_at_d <- 1.0
  } else {
    last_idx <- eligible_rows[length(eligible_rows)]
    surv_at_d <- stab$surv[last_idx]
  }

  surv_at_d <- max(0.0, min(1.0, surv_at_d))

  # Evaluate Estimability Rules:
  # Case D: Survival reached 0 at or before t_duration
  if (surv_at_d == 0.0 && n_events > 0) {
    estimable <- TRUE
    rel_pos <- 1.0
  } else if (n_events == 0) {
    # Case B / no events: All neighbours censored, no resolution experience observed
    estimable <- FALSE
    rel_pos <- NA_real_
    surv_at_d <- NA_real_
  } else if (t_duration > max_followup) {
    # Case B: Target duration exceeds maximum follow-up of all neighbours (and S > 0)
    estimable <- FALSE
    rel_pos <- NA_real_
    surv_at_d <- NA_real_
  } else {
    # Case A / Case C: Supported within follow-up range with observed resolution events
    estimable <- TRUE
    rel_pos <- 1.0 - surv_at_d
  }

  # Construct non-causal interpretation string
  if (!estimable) {
    if (n_events == 0) {
      interp <- sprintf(
        "Relative position is not estimable for case '%s': all %d neighbouring cases are censored without any observed resolution events.",
        target_id, n_neigh
      )
    } else {
      interp <- sprintf(
        "Relative position is not estimable for case '%s': target duration (%g days) exceeds the maximum follow-up duration of neighbouring cases (%g days).",
        target_id, t_duration, max_followup
      )
    }
  } else {
    pct_val <- round(rel_pos * 100, 1)
    if (identical(target_status, "pending")) {
      interp <- sprintf(
        "At its current age of %g days, the pending case is older than approximately %.1f%% of the weighted legally similar resolution experience.",
        t_duration, pct_val
      )
    } else if (identical(target_status, "disposed")) {
      interp <- sprintf(
        "Retrospectively, at its resolution duration of %g days, the case was older than approximately %.1f%% of the weighted legally similar resolution experience.",
        t_duration, pct_val
      )
    } else {
      interp <- sprintf(
        "At duration of %g days, the case is older than approximately %.1f%% of the weighted legally similar resolution experience.",
        t_duration, pct_val
      )
    }
  }

  res <- list(
    target_case_id = as.character(target_id),
    target_status = as.character(target_status),
    target_duration = t_duration,
    survival_at_duration = surv_at_d,
    relative_position = rel_pos,
    estimable = estimable,
    interpretation = interp,
    n_neighbours = n_neigh,
    median_neighbourhood_survival = med_surv,
    neighbourhood = neighbourhood
  )
  class(res) <- "relative_lateness"
  res
}

#' Print method for relative_lateness
#' @param x A \code{relative_lateness} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print relative_lateness
print.relative_lateness <- function(x, ...) {
  cat("Relative Case Lateness\n")
  cat("======================\n")
  cat("Target Case ID:              ", x$target_case_id, "\n")
  cat("Target Status:               ", x$target_status, "\n")
  cat("Target Duration:             ", x$target_duration, "days\n")
  cat("Valid Neighbours:            ", x$n_neighbours, "\n")
  cat("Estimable:                   ", x$estimable, "\n")
  if (!is.na(x$median_neighbourhood_survival)) {
    cat("Median Neighbourhood Survival:", x$median_neighbourhood_survival, "days\n")
  } else {
    cat("Median Neighbourhood Survival: Not reached\n")
  }
  if (isTRUE(x$estimable)) {
    cat("Survival at Target Duration: ", round(x$survival_at_duration, 4), "\n")
    cat("Relative Position:           ", round(x$relative_position, 4), " (", round(x$relative_position * 100, 1), "th percentile)\n", sep = "")
  } else {
    cat("Survival at Target Duration:  NA\n")
    cat("Relative Position:            NA\n")
  }
  cat("\nInterpretation:\n", x$interpretation, "\n", sep = "")
  invisible(x)
}
