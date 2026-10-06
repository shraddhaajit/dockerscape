#' Standardise and Validate Case Records for Delay and Survival Analysis
#'
#' Takes raw case-level data containing filing dates and optional decision dates,
#' validates dates, flags problematic records without deletion, and calculates
#' right-censored survival duration and event indicators relative to an observation end date.
#'
#' @param cases A \code{data.frame} containing case-level records.
#' @param filing Character string specifying the name of the column containing filing dates, or a vector of filing dates matching \code{nrow(cases)}. Defaults to \code{"filing_date"}.
#' @param decision Character string specifying the name of the column containing decision dates, or a vector of decision dates matching \code{nrow(cases)}. Defaults to \code{"decision_date"}.
#' @param observation_end A single Date object or character string coercible to Date (e.g. \code{"2014-12-31"}) representing the cutoff date of observation.
#'
#' @return A modified \code{data.frame} based on \code{cases} with additional or updated columns:
#' \describe{
#'   \item{filing_date}{Standardised \code{Date} object for case filing.}
#'   \item{decision_date}{Standardised \code{Date} object for case decision (NA if pending).}
#'   \item{duration}{Numeric duration in days. For disposed cases, \code{decision_date - filing_date}. For pending cases, \code{observation_end - filing_date}. Set to \code{NA_real_} for invalid records.}
#'   \item{event}{Numeric event status indicator. \code{1} for disposed, \code{0} for pending, \code{NA_real_} for invalid records.}
#'   \item{is_valid}{Logical flag indicating whether the record is valid (\code{TRUE}) or problematic (\code{FALSE}).}
#'   \item{invalid_reason}{Character string describing the issue if \code{is_valid == FALSE}, otherwise \code{NA_character_}.}
#' }
#'
#' @details
#' \bold{Censoring Logic:}
#' \itemize{
#'   \item Disposed case: \code{decision_date <= observation_end}. \code{duration = decision_date - filing_date}, \code{event = 1}.
#'   \item Pending case: \code{is.na(decision_date)}. \code{duration = observation_end - filing_date}, \code{event = 0}.
#'   \item Invalid case: Cases with missing filing dates, decision dates before filing dates, or filing dates after \code{observation_end} are flagged with \code{is_valid = FALSE} and an explicit \code{invalid_reason}. They are NOT deleted. Durations for invalid records are set to \code{NA} to ensure no negative durations exist in downstream statistical models.
#' }
#'
#' @examples
#' cases <- data.frame(
#'   case_id = 1:3,
#'   filing_date = as.Date(c("2012-01-10", "2012-03-05", "2012-06-20")),
#'   decision_date = as.Date(c("2012-11-02", "2013-02-14", NA))
#' )
#' clean_cases(cases, observation_end = "2014-12-31")
#'
#' @export
clean_cases <- function(cases,
                        filing = "filing_date",
                        decision = "decision_date",
                        observation_end) {
  if (!is.data.frame(cases)) {
    stop("`cases` must be a data.frame.", call. = FALSE)
  }

  if (missing(observation_end) || is.null(observation_end) || is.na(observation_end)) {
    stop("`observation_end` must be provided as a single valid Date or date string.", call. = FALSE)
  }

  obs_end <- tryCatch(
    as.Date(observation_end),
    error = function(e) NA
  )
  if (length(obs_end) != 1 || is.na(obs_end)) {
    stop("`observation_end` could not be converted to a single valid Date object.", call. = FALSE)
  }

  # Extract or parse filing date column
  if (is.character(filing) && length(filing) == 1 && filing %in% names(cases)) {
    raw_filing <- cases[[filing]]
  } else if (length(filing) == nrow(cases)) {
    raw_filing <- filing
  } else {
    stop(sprintf("Column '%s' not found in `cases`.", as.character(filing)[1]), call. = FALSE)
  }

  # Extract or parse decision date column
  if (is.character(decision) && length(decision) == 1 && decision %in% names(cases)) {
    raw_decision <- cases[[decision]]
  } else if (length(decision) == nrow(cases)) {
    raw_decision <- decision
  } else {
    stop(sprintf("Column '%s' not found in `cases`.", as.character(decision)[1]), call. = FALSE)
  }

  # Standardise dates
  filing_date <- as.Date(raw_filing)
  decision_date <- as.Date(raw_decision)

  n <- nrow(cases)
  is_valid <- rep(TRUE, n)
  invalid_reason <- rep(NA_character_, n)
  duration <- rep(NA_real_, n)
  event <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    f_date <- filing_date[i]
    d_date <- decision_date[i]

    # Check 1: Missing filing date
    if (is.na(f_date)) {
      is_valid[i] <- FALSE
      invalid_reason[i] <- "Missing filing date"
      next
    }

    # Check 2: Case filed after observation_end
    if (f_date > obs_end) {
      is_valid[i] <- FALSE
      invalid_reason[i] <- "Filing date after observation_end"
      next
    }

    # Check 3: Decision date before filing date
    if (!is.na(d_date) && d_date < f_date) {
      is_valid[i] <- FALSE
      invalid_reason[i] <- "Decision date before filing date"
      next
    }

    # Check 4: Decision date after observation_end
    if (!is.na(d_date) && d_date > obs_end) {
      is_valid[i] <- FALSE
      invalid_reason[i] <- "Decision date after observation_end"
      next
    }

    # If valid, calculate event and duration
    if (!is.na(d_date)) {
      # Disposed case
      event[i] <- 1
      duration[i] <- as.numeric(d_date - f_date)
    } else {
      # Pending case (right-censored at observation_end)
      event[i] <- 0
      duration[i] <- as.numeric(obs_end - f_date)
    }
  }

  res <- cases
  res$filing_date <- filing_date
  res$decision_date <- decision_date
  res$duration <- duration
  res$event <- event
  res$is_valid <- is_valid
  res$invalid_reason <- invalid_reason

  res
}
