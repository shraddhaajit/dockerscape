#' Fit Censoring-Aware Delay Model
#'
#' Fits a Cox proportional hazards model relating case resolution speed to the case's
#' legal similarity profile score and its court's docket load at filing time.
#'
#' @param cases_clean A \code{data.frame} of cleaned case records (produced by \code{\link{clean_cases}}).
#' @param case_graph An \code{igraph} graph of case similarities (produced by \code{\link{build_case_graph}}).
#' @param court_load_df A \code{court_load} data frame (produced by \code{\link{court_load}}) containing columns \code{court}, \code{date}, and \code{load}.
#' @param court Character string specifying the column name for court identifiers in \code{cases_clean}. Defaults to \code{"court"}.
#'
#' @return An S3 object of class \code{c("delay_model", "list")} containing:
#' \describe{
#'   \item{model}{The fitted \code{\link[survival]{coxph}} model object (or \code{NULL} if fitting failed).}
#'   \item{model_data}{The subset \code{data.frame} used for model fitting.}
#'   \item{coefficients}{A data frame of estimated coefficients, standard errors, z-scores, and p-values.}
#'   \item{hazard_ratios}{A data frame of hazard ratios (\eqn{\exp(\beta)}) and 95\% confidence intervals.}
#'   \item{n_obs}{Total number of valid observations included in the model.}
#'   \item{n_events}{Number of resolution events (\code{event == 1}) in the model data.}
#'   \item{n_excluded}{Total number of cases excluded due to missing data or unmapped predictors.}
#'   \item{exclusions}{Named integer vector showing breakdown of exclusion reasons.}
#'   \item{legal_score_def}{Character string describing the legal score calculation method.}
#'   \item{load_def}{Character string describing filing-time court docket load alignment.}
#'   \item{formula}{The \code{\link[stats]{formula}} used in fitting.}
#'   \item{ph_test}{Output of \code{\link[survival]{cox.zph}} diagnostic test if feasible, otherwise \code{NULL}.}
#'   \item{fit_success}{Logical indicating whether the Cox regression model fitted successfully.}
#'   \item{fit_message}{Character description of model status or error message if fitting failed.}
#' }
#'
#' @details
#' \bold{Legal Score Definition:}
#' For case \eqn{i}, \code{legal_score} is calculated as the mean weight of all incident edges in \code{case_graph}:
#' \deqn{\text{legal\_score}_i = \frac{1}{|N(i)|} \sum_{j \in N(i)} w_{ij}}
#' If a case has no legal graph neighbours, \code{legal_score} is assigned \code{NA} (not 0).
#'
#' \bold{Filing-Time Court Load Alignment:}
#' Each case's filing date is mapped to the first of its calendar month (\code{format(filing_date, "\%Y-\%m-01")}),
#' and matched against the court's docket load from \code{court_load_df}.
#' \code{load_at_filing} represents baseline docket exposure at filing, not time-varying cumulative load across case lifetime.
#'
#' \bold{Statistical Interpretation:}
#' Model estimates quantify observational associations. A hazard ratio \eqn{\text{HR} > 1} for \code{load_at_filing} indicates that
#' higher baseline docket load is associated with a higher hazard of resolution (faster resolution), conditional on legal score.
#' Model coefficients are non-causal associations and must NOT be interpreted as causal delay impacts or judge performance measures.
#'
#' @examples
#' \dontrun{
#' cleaned <- clean_cases(example_cases)
#' s_graph <- build_section_graph(example_sections)
#' diff_prof <- diffuse_profiles(example_profiles, s_graph)
#' c_graph <- build_case_graph(diff_prof)
#' load_df <- court_load(cleaned, by = "month")
#' fit <- fit_delay_model(cleaned, c_graph, load_df)
#' print(fit)
#' }
#'
#' @export
fit_delay_model <- function(cases_clean,
                            case_graph,
                            court_load_df,
                            court = "court") {
  if (!is.data.frame(cases_clean)) {
    stop("`cases_clean` must be a data.frame.", call. = FALSE)
  }

  req_cols <- c("case_id", "filing_date", "event", "duration")
  missing_cols <- setdiff(req_cols, names(cases_clean))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required column(s) missing from `cases_clean`: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

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

  if (!is.data.frame(court_load_df) || !all(c("court", "date", "load") %in% names(court_load_df))) {
    stop("`court_load_df` must be a data frame containing columns 'court', 'date', and 'load'.", call. = FALSE)
  }

  n_total <- nrow(cases_clean)
  case_ids <- as.character(cases_clean$case_id)

  # 1. Compute Legal Score per case
  legal_scores <- rep(NA_real_, n_total)
  if (inherits(case_graph, "igraph") && igraph::vcount(case_graph) > 0) {
    v_names <- igraph::V(case_graph)$name
    if (!is.null(v_names)) {
      w_attr <- if ("weight" %in% igraph::edge_attr_names(case_graph)) igraph::E(case_graph)$weight else NULL
      deg <- igraph::degree(case_graph, mode = "all")
      str_val <- if (!is.null(w_attr)) igraph::strength(case_graph, mode = "all", weights = w_attr) else as.numeric(deg)
      v_scores <- ifelse(deg > 0, str_val / deg, NA_real_)
      v_map <- match(case_ids, v_names)
      has_match <- !is.na(v_map)
      legal_scores[has_match] <- v_scores[v_map[has_match]]
    }
  }

  # 2. Align Filing-Time Court Load
  filing_months <- as.Date(format(as.Date(cases_clean$filing_date), "%Y-%m-01"))
  cl_court <- as.character(court_load_df$court)
  cl_date <- as.Date(court_load_df$date)
  cl_load <- court_load_df$load

  load_at_filing <- rep(NA_real_, n_total)
  key_cl <- paste0(cl_court, "||", cl_date)
  key_cases <- paste0(court_vals, "||", filing_months)
  match_idx <- match(key_cases, key_cl)
  valid_m <- !is.na(court_vals) & !is.na(filing_months) & !is.na(match_idx)
  load_at_filing[valid_m] <- cl_load[match_idx[valid_m]]

  # 3. Track Row Inclusion / Exclusions
  is_valid_case <- rep(TRUE, n_total)
  if ("is_valid" %in% names(cases_clean)) {
    is_valid_case <- (cases_clean$is_valid == TRUE)
  }

  has_duration_event <- !is.na(cases_clean$duration) & cases_clean$duration > 0 &
                        !is.na(cases_clean$event) & cases_clean$event %in% c(0, 1)
  has_court <- !is.na(court_vals) & nchar(trimws(court_vals)) > 0
  has_legal_score <- !is.na(legal_scores)
  has_load <- !is.na(load_at_filing)

  include_mask <- is_valid_case & has_duration_event & has_court & has_legal_score & has_load

  excl_invalid <- sum(!is_valid_case)
  excl_dur_evt <- sum(is_valid_case & !has_duration_event)
  excl_court <- sum(is_valid_case & has_duration_event & !has_court)
  excl_legal <- sum(is_valid_case & has_duration_event & has_court & !has_legal_score)
  excl_load <- sum(is_valid_case & has_duration_event & has_court & has_legal_score & !has_load)

  exclusions_vec <- c(
    invalid_record = excl_invalid,
    missing_duration_or_event = excl_dur_evt,
    missing_court = excl_court,
    missing_legal_score = excl_legal,
    missing_court_load = excl_load
  )
  n_excluded <- sum(!include_mask)

  model_data <- data.frame(
    case_id = case_ids[include_mask],
    court = court_vals[include_mask],
    filing_date = as.Date(cases_clean$filing_date[include_mask]),
    duration = cases_clean$duration[include_mask],
    event = cases_clean$event[include_mask],
    legal_score = legal_scores[include_mask],
    load_at_filing = load_at_filing[include_mask],
    stringsAsFactors = FALSE
  )

  form <- survival::Surv(duration, event) ~ legal_score + load_at_filing

  legal_def_str <- "Mean similarity weight of incident edges in case_graph (NA if no neighbours)"
  load_def_str <- "Open court docket load at case filing month (floor_date to 1st of month)"

  # Check if model data is sufficient for Cox fitting
  if (nrow(model_data) == 0) {
    res <- list(
      model = NULL,
      model_data = model_data,
      coefficients = data.frame(),
      hazard_ratios = data.frame(),
      n_obs = 0,
      n_events = 0,
      n_excluded = n_excluded,
      exclusions = exclusions_vec,
      legal_score_def = legal_def_str,
      load_def = load_def_str,
      formula = form,
      ph_test = NULL,
      fit_success = FALSE,
      fit_message = "No valid observations remaining after exclusions."
    )
    class(res) <- c("delay_model", "list")
    return(res)
  }

  n_obs <- nrow(model_data)
  n_events <- sum(model_data$event == 1)

  if (n_obs < 3) {
    res <- list(
      model = NULL,
      model_data = model_data,
      coefficients = data.frame(),
      hazard_ratios = data.frame(),
      n_obs = n_obs,
      n_events = n_events,
      n_excluded = n_excluded,
      exclusions = exclusions_vec,
      legal_score_def = legal_def_str,
      load_def = load_def_str,
      formula = form,
      ph_test = NULL,
      fit_success = FALSE,
      fit_message = sprintf("Insufficient observations (n = %d < 3) to fit multivariable Cox regression.", n_obs)
    )
    class(res) <- c("delay_model", "list")
    return(res)
  }

  if (n_events == 0) {
    res <- list(
      model = NULL,
      model_data = model_data,
      coefficients = data.frame(),
      hazard_ratios = data.frame(),
      n_obs = n_obs,
      n_events = 0,
      n_excluded = n_excluded,
      exclusions = exclusions_vec,
      legal_score_def = legal_def_str,
      load_def = load_def_str,
      formula = form,
      ph_test = NULL,
      fit_success = FALSE,
      fit_message = "Zero resolution events present in model data; Cox regression cannot be fitted."
    )
    class(res) <- c("delay_model", "list")
    return(res)
  }

  # Fit Cox Proportional Hazards Model safely
  cox_fit <- NULL
  fit_err <- NULL
  fit_warn <- NULL

  tryCatch({
    withCallingHandlers({
      cox_fit <- survival::coxph(form, data = model_data)
    }, warning = function(w) {
      fit_warn <<- w$message
      invokeRestart("muffleWarning")
    })
  }, error = function(e) {
    fit_err <<- e$message
  })

  if (!is.null(fit_warn) && grepl("did not converge", fit_warn, ignore.case = TRUE)) {
    res <- list(
      model = cox_fit,
      model_data = model_data,
      coefficients = data.frame(),
      hazard_ratios = data.frame(),
      n_obs = n_obs,
      n_events = n_events,
      n_excluded = n_excluded,
      exclusions = exclusions_vec,
      legal_score_def = legal_def_str,
      load_def = load_def_str,
      formula = form,
      ph_test = NULL,
      fit_success = FALSE,
      fit_message = sprintf("Cox model fitting did not converge: %s", fit_warn)
    )
    class(res) <- c("delay_model", "list")
    return(res)
  }

  if (is.null(cox_fit) || !is.null(fit_err)) {
    res <- list(
      model = NULL,
      model_data = model_data,
      coefficients = data.frame(),
      hazard_ratios = data.frame(),
      n_obs = n_obs,
      n_events = n_events,
      n_excluded = n_excluded,
      exclusions = exclusions_vec,
      legal_score_def = legal_def_str,
      load_def = load_def_str,
      formula = form,
      ph_test = NULL,
      fit_success = FALSE,
      fit_message = sprintf("Cox model fitting failed: %s", fit_err)
    )
    class(res) <- c("delay_model", "list")
    return(res)
  }

  # Extract coefficients, HRs, CIs
  s_fit <- summary(cox_fit)
  coef_mat <- s_fit$coefficients
  conf_mat <- s_fit$conf.int

  coef_df <- data.frame(
    term = rownames(coef_mat),
    estimate = coef_mat[, "coef"],
    std_error = coef_mat[, "se(coef)"],
    statistic = coef_mat[, "z"],
    p_value = coef_mat[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  rownames(coef_df) <- NULL

  hr_df <- data.frame(
    term = rownames(conf_mat),
    hazard_ratio = conf_mat[, "exp(coef)"],
    conf_low = conf_mat[, "lower .95"],
    conf_high = conf_mat[, "upper .95"],
    stringsAsFactors = FALSE
  )
  rownames(hr_df) <- NULL

  # ----------------------------------------------------------------
  # NUMERICAL STABILITY CHECK -- must occur BEFORE declaring success
  # ----------------------------------------------------------------
  # 1. Detect near-zero variance predictors (SD < 1e-10)
  legal_sd  <- stats::sd(model_data$legal_score,    na.rm = TRUE)
  load_sd   <- stats::sd(model_data$load_at_filing, na.rm = TRUE)
  near_zero_vars <- character(0)
  if (!is.na(legal_sd)  && legal_sd  < 1e-10) near_zero_vars <- c(near_zero_vars, "legal_score")
  if (!is.na(load_sd)   && load_sd   < 1e-10) near_zero_vars <- c(near_zero_vars, "load_at_filing")

  # 2. Detect extreme or NA coefficients  (|beta| > 1e6 or NA)
  raw_coefs        <- stats::coef(cox_fit)
  extreme_coefs    <- names(raw_coefs)[!is.na(raw_coefs) & abs(raw_coefs) > 1e6]
  na_coefs         <- names(raw_coefs)[is.na(raw_coefs)]

  instability_reasons <- character(0)
  if (length(near_zero_vars) > 0)
    instability_reasons <- c(instability_reasons,
      sprintf("Near-zero variance predictor(s): %s (SD < 1e-10 indicates effective constant)",
              paste(near_zero_vars, collapse = ", ")))
  if (length(na_coefs) > 0)
    instability_reasons <- c(instability_reasons,
      sprintf("NA coefficient(s) -- rank-deficient information matrix: %s",
              paste(na_coefs, collapse = ", ")))
  if (length(extreme_coefs) > 0)
    instability_reasons <- c(instability_reasons,
      sprintf("Numerically extreme coefficient(s) |beta| > 1e6: %s = %s",
              paste(extreme_coefs, collapse = ", "),
              paste(formatC(raw_coefs[extreme_coefs], format = "e", digits = 3), collapse = ", ")))

  if (length(instability_reasons) > 0) {
    res <- list(
      model = cox_fit,
      model_data = model_data,
      coefficients = coef_df,
      hazard_ratios = hr_df,
      n_obs = n_obs,
      n_events = n_events,
      n_excluded = n_excluded,
      exclusions = exclusions_vec,
      legal_score_def = legal_def_str,
      load_def = load_def_str,
      formula = form,
      ph_test = NULL,
      case_graph = case_graph,
      fit_success = FALSE,
      fit_message = paste(
        "Cox model fitted but NUMERICALLY UNSTABLE -- estimates are unreliable:",
        paste(instability_reasons, collapse = "; ")
      ),
      numerical_instability = TRUE,
      instability_reasons = instability_reasons
    )
    class(res) <- c("delay_model", "list")
    return(res)
  }

  # Test Proportional Hazards Assumption (only when model is stable)
  ph_test_res <- NULL
  tryCatch({
    ph_test_res <- survival::cox.zph(cox_fit)
  }, error = function(e) {
    ph_test_res <<- NULL
  })

  res <- list(
    model = cox_fit,
    model_data = model_data,
    coefficients = coef_df,
    hazard_ratios = hr_df,
    n_obs = n_obs,
    n_events = n_events,
    n_excluded = n_excluded,
    exclusions = exclusions_vec,
    legal_score_def = legal_def_str,
    load_def = load_def_str,
    formula = form,
    ph_test = ph_test_res,
    case_graph = case_graph,
    fit_success = TRUE,
    fit_message = "Cox proportional hazards model fitted successfully.",
    numerical_instability = FALSE,
    instability_reasons = character(0)
  )
  class(res) <- c("delay_model", "list")
  res
}

#' Print method for delay_model
#' @param x A \code{delay_model} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print delay_model
print.delay_model <- function(x, ...) {
  cat("Docketscape Delay Model (Cox Proportional Hazards)\n")
  cat("=================================================\n")
  cat("Status:       ", x$fit_message, "\n")
  cat("Observations: ", x$n_obs, " included, ", x$n_excluded, " excluded\n", sep = "")
  cat("Events:       ", x$n_events, " resolutions observed\n\n")

  if (x$n_excluded > 0) {
    cat("Exclusion Breakdown:\n")
    excl_active <- x$exclusions[x$exclusions > 0]
    for (nm in names(excl_active)) {
      cat(sprintf("  - %s: %d\n", nm, excl_active[[nm]]))
    }
    cat("\n")
  }

  if (x$fit_success && !is.null(x$model)) {
    cat("Hazard Ratios & 95% Confidence Intervals:\n")
    print(x$hazard_ratios)
    cat("\nCoefficient Estimates:\n")
    print(x$coefficients)
    cat("\n")

    if (!is.null(x$ph_test)) {
      cat("Proportional Hazards Assumption Test (cox.zph):\n")
      print(x$ph_test)
      cat("\n")
    } else {
      cat("Proportional Hazards Test (cox.zph): Not evaluated or insufficient sample size.\n\n")
    }
  }

  cat("Note: Hazard ratios quantify observational baseline associations with resolution hazard;\n")
  cat("they do NOT represent causal delay impacts or judicial performance metrics.\n")
  invisible(x)
}
