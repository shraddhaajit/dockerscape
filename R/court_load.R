#' Calculate Time-Varying Court Docket Load
#'
#' Computes the number of open cases at each court across specified time points (monthly or daily).
#' A case contributes to court load at time \eqn{t} if it was filed on or before \eqn{t} and
#' has not yet been disposed by \eqn{t}.
#'
#' @param cases_clean A \code{data.frame} of cleaned case records (produced by \code{\link{clean_cases}}).
#' @param court Character string specifying the column name for court identifiers. Defaults to \code{"court"}.
#' @param by Character string specifying the time aggregation interval: \code{"month"} (default) or \code{"day"}.
#'
#' @return An S3 object of class \code{c("court_load", "data.frame")} containing:
#' \describe{
#'   \item{court}{Character column containing court identifiers.}
#'   \item{date}{\code{Date} object representing the evaluation time point (1st of each month for \code{by = "month"}).}
#'   \item{load}{Integer count of open cases at that court and date.}
#' }
#'
#' @details
#' \bold{Open-Case Boundary Convention:}
#' A case is defined as open at court \eqn{c} at time \eqn{t} if:
#' \deqn{\text{filing\_date} \le t \quad \text{AND} \quad (\text{event} == 0 \quad \text{OR} \quad \text{decision\_date} > t)}
#'
#' \itemize{
#'   \item \bold{Disposed Cases (\eqn{event = 1}):} Contribute to load starting on their filing date up to, but excluding, their decision date. A case disposed on date \eqn{t} is considered no longer open at time point \eqn{t}.
#'   \item \bold{Pending Cases (\eqn{event = 0}):} Remain open at all time points on or after their filing date through the end of the observation window.
#'   \item \bold{Time Points:} For \code{by = "month"}, time points are set to the 1st of each calendar month (e.g. \code{2020-01-01}, \code{2020-02-01}).
#' }
#'
#' \bold{Statistical Interpretation:}
#' \code{court_load()} measures docket exposure / open case volume over time. It does NOT measure judge efficiency, judicial quality, or causal congestion effects.
#'
#' @examples
#' cases <- data.frame(
#'   case_id = 1:3,
#'   court = c("C1", "C1", "C2"),
#'   filing_date = as.Date(c("2012-01-10", "2012-03-05", "2012-06-20")),
#'   decision_date = as.Date(c("2012-11-02", "2013-02-14", NA))
#' )
#' cleaned <- clean_cases(cases, observation_end = "2014-12-31")
#' load_df <- court_load(cleaned, by = "month")
#' head(load_df)
#'
#' @export
court_load <- function(cases_clean,
                       court = "court",
                       by = "month") {
  if (!is.data.frame(cases_clean)) {
    stop("`cases_clean` must be a data.frame.", call. = FALSE)
  }

  if (!by %in% c("month", "day")) {
    stop("`by` must be either 'month' or 'day'.", call. = FALSE)
  }

  # Extract court column
  if (is.character(court) && length(court) == 1) {
    if (court %in% names(cases_clean)) {
      court_vals <- as.character(cases_clean[[court]])
    } else {
      stop(sprintf("Column '%s' not found in `cases_clean`.", court), call. = FALSE)
    }
  } else if (length(court) == nrow(cases_clean)) {
    court_vals <- as.character(court)
  } else {
    stop(sprintf("Column '%s' not found in `cases_clean`.", as.character(court)[1]), call. = FALSE)
  }

  req_cols <- c("filing_date", "event", "duration")
  missing_cols <- setdiff(req_cols, names(cases_clean))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required column(s) missing from `cases_clean`: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  # Filter valid rows
  valid_mask <- !is.na(cases_clean$filing_date) &
                !is.na(cases_clean$event) &
                !is.na(cases_clean$duration) & cases_clean$duration >= 0 &
                !is.na(court_vals) & nchar(trimws(court_vals)) > 0

  if ("is_valid" %in% names(cases_clean)) {
    valid_mask <- valid_mask & (cases_clean$is_valid == TRUE)
  }

  if (!any(valid_mask)) {
    empty_res <- data.frame(
      court = character(0),
      date = as.Date(character(0)),
      load = integer(0),
      stringsAsFactors = FALSE
    )
    class(empty_res) <- c("court_load", "data.frame")
    return(empty_res)
  }

  clean_df <- data.frame(
    court = trimws(court_vals[valid_mask]),
    filing_date = as.Date(cases_clean$filing_date[valid_mask]),
    decision_date = if ("decision_date" %in% names(cases_clean)) as.Date(cases_clean$decision_date[valid_mask]) else as.Date(NA),
    event = cases_clean$event[valid_mask],
    duration = cases_clean$duration[valid_mask],
    stringsAsFactors = FALSE
  )

  # Infer endpoint date per case (decision_date if event == 1, else filing_date + duration)
  case_endpoints <- ifelse(
    clean_df$event == 1 & !is.na(clean_df$decision_date),
    clean_df$decision_date,
    clean_df$filing_date + clean_df$duration
  )
  case_endpoints <- as.Date(case_endpoints, origin = "1970-01-01")

  # Overall time range
  min_f_date <- min(clean_df$filing_date)
  max_r_date <- max(case_endpoints)

  if (by == "month") {
    start_date <- as.Date(format(min_f_date, "%Y-%m-01"))
    end_date <- as.Date(format(max_r_date, "%Y-%m-01"))
    date_seq <- seq.Date(from = start_date, to = end_date, by = "month")
  } else {
    date_seq <- seq.Date(from = min_f_date, to = max_r_date, by = "day")
  }

  unique_courts <- sort(unique(clean_df$court))
  n_dates <- length(date_seq)

  out_list <- list()
  idx <- 1

  for (c_id in unique_courts) {
    sub_df <- clean_df[clean_df$court == c_id, , drop = FALSE]
    f_vec <- sort(sub_df$filing_date)
    disp_cases <- sub_df[sub_df$event == 1 & !is.na(sub_df$decision_date), , drop = FALSE]
    d_vec <- if (nrow(disp_cases) > 0) sort(disp_cases$decision_date) else as.Date(character(0))

    n_f <- findInterval(date_seq, f_vec)
    n_d <- if (length(d_vec) > 0) findInterval(date_seq, d_vec) else 0L
    load_counts <- n_f - n_d

    out_list[[idx]] <- data.frame(
      court = rep(c_id, n_dates),
      date = date_seq,
      load = as.integer(load_counts),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }

  res <- do.call(rbind, out_list)
  res <- res[order(res$court, res$date), , drop = FALSE]
  rownames(res) <- NULL

  class(res) <- c("court_load", "data.frame")
  res
}

#' Print method for court_load
#' @param x A \code{court_load} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print court_load
print.court_load <- function(x, ...) {
  if (!inherits(x, "data.frame") || nrow(x) == 0) {
    cat("Court Load (Empty)\n")
    return(invisible(x))
  }
  courts <- unique(x$court)
  cat("Court Docket Load Summary\n")
  cat("=========================\n")
  cat("Number of courts: ", length(courts), " (", paste(head(courts, 5), collapse = ", "), if(length(courts) > 5) "..." else "", ")\n", sep = "")
  cat("Date range:       ", as.character(min(x$date)), " to ", as.character(max(x$date)), "\n")
  cat("Min load:         ", min(x$load), "\n")
  cat("Max load:         ", max(x$load), "\n\n")

  cat("Head of court_load data:\n")
  print(head(as.data.frame(x), 10))
  invisible(x)
}
