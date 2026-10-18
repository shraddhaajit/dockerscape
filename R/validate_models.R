#' Project Validation Case Legal Profiles Against Training Representation Space
#'
#' Projects validation cases onto a training-derived legal section representation space using
#' training section graph diffusion, training IDF weights, and similarity against training cases only.
#' This ensures validation case legal scores are calculated strictly from training information,
#' preventing future-neighbour leakage and validation-to-validation contamination.
#'
#' @param val_sections A \code{data.frame} of legal section records for validation cases (must contain \code{case_id}, \code{act}, \code{section}).
#' @param train_sections A \code{data.frame} of legal section records for training cases (must contain \code{case_id}, \code{act}, \code{section}).
#' @param k Positive integer specifying the maximum number of training neighbours each validation case retains. Defaults to \code{3}.
#' @param use_idf Logical flag indicating whether to apply training inverse document frequency (IDF) weighting. Defaults to \code{TRUE}.
#' @param alpha Numeric diffusion parameter in \code{[0, 1]}. Defaults to \code{0.7}.
#'
#' @return A \code{list} containing:
#' \describe{
#'   \item{validation_legal_scores}{Named numeric vector of projected legal scores for validation cases.}
#'   \item{unseen_section_cases}{Character vector of validation case IDs containing sections absent from the training section vocabulary.}
#'   \item{val_cases_using_train_neighbours}{Count of validation cases with at least 1 training neighbour.}
#'   \item{val_cases_using_val_neighbours}{Count of validation cases using validation neighbours (always \code{0L} by design).}
#'   \item{neighbour_map}{List mapping each validation case ID to a data frame of its top training neighbours and similarity weights.}
#' }
#'
#' @details
#' \bold{Information Boundary & Projection Mechanics:}
#' \enumerate{
#'   \item \bold{Training Representation:} The section graph, section transition matrix \eqn{P_{train}}, and IDF weights \eqn{w_{train}} are constructed exclusively from \code{train_sections}.
#'   \item \bold{Unseen Sections:} Any section present in a validation case that was absent from \code{train_sections} is assigned zero contribution (ignored in representation space) without modifying training vocabulary or matrices.
#'   \item \bold{Independent Projection:} Each validation case is projected independently against training cases. Validation cases cannot form edges to each other (\code{val_cases_using_val_neighbours = 0}).
#' }
#'
#' @examples
#' train_sec <- data.frame(
#'   case_id = c("1", "2", "2"),
#'   act = c("IPC", "IPC", "CrPC"),
#'   section = c("420", "420", "154"),
#'   stringsAsFactors = FALSE
#' )
#' val_sec <- data.frame(
#'   case_id = c("3", "3"),
#'   act = c("IPC", "NI"),
#'   section = c("420", "138"),
#'   stringsAsFactors = FALSE
#' )
#' proj <- project_validation_legal_score(val_sec, train_sec, k = 2)
#' print(proj$validation_legal_scores)
#'
#' @export
project_validation_legal_score <- function(val_sections,
                                           train_sections,
                                           k = 3,
                                           use_idf = TRUE,
                                           alpha = 0.7) {
  if (!is.data.frame(val_sections) || !is.data.frame(train_sections)) {
    stop("`val_sections` and `train_sections` must be data frames.", call. = FALSE)
  }

  req_cols <- c("case_id", "act", "section")
  if (!all(req_cols %in% names(val_sections)) || !all(req_cols %in% names(train_sections))) {
    stop("`val_sections` and `train_sections` must contain 'case_id', 'act', and 'section'.", call. = FALSE)
  }

  # 1. Build Training Section Graph and Training Diffused Profiles
  train_sec_graph <- build_section_graph(train_sections)
  train_diff_p <- diffuse_profiles(train_sections, train_sec_graph, alpha = alpha)

  W_train_raw <- train_diff_p$profiles
  train_case_ids <- train_diff_p$case_ids
  train_sec_ids <- train_diff_p$section_ids

  n_train <- length(train_case_ids)
  m_train <- length(train_sec_ids)

  if (n_train == 0 || m_train == 0) {
    val_ids <- unique(as.character(val_sections$case_id))
    scores <- stats::setNames(rep(NA_real_, length(val_ids)), val_ids)
    return(list(
      validation_legal_scores = scores,
      unseen_section_cases = val_ids,
      val_cases_using_train_neighbours = 0L,
      val_cases_using_val_neighbours = 0L,
      neighbour_map = list()
    ))
  }

  # 2. Compute Training IDF Weights
  if (use_idf) {
    df_s <- Matrix::colSums(W_train_raw > 0)
    idf_train <- ifelse(df_s > 0, log(n_train / df_s) + 1, 0)
    W_train_idf <- W_train_raw %*% Matrix::Diagonal(x = idf_train)
  } else {
    idf_train <- rep(1, m_train)
    W_train_idf <- W_train_raw
  }

  # Row L2 normalize training profiles
  tr_row_l2 <- sqrt(Matrix::rowSums(W_train_idf^2))
  tr_inv_l2 <- ifelse(tr_row_l2 > 0, 1 / tr_row_l2, 0)
  W_train_norm <- Matrix::Diagonal(x = tr_inv_l2) %*% W_train_idf

  # 3. Construct Training Transition Matrix P_train
  if (igraph::vcount(train_sec_graph) > 0) {
    A_train <- igraph::as_adjacency_matrix(train_sec_graph, attr = "weight", sparse = TRUE)
    A_train <- A_train[train_sec_ids, train_sec_ids, drop = FALSE]
  } else {
    A_train <- Matrix::Matrix(0, nrow = m_train, ncol = m_train, sparse = TRUE)
  }

  deg_tr <- Matrix::rowSums(A_train)
  inv_deg_tr <- ifelse(deg_tr > 0, 1 / deg_tr, 0)
  P_train <- Matrix::Diagonal(x = inv_deg_tr) %*% A_train

  # 4. Clean Validation Sections
  val_clean <- val_sections[
    !is.na(val_sections$case_id) &
    !is.na(val_sections$act) & nchar(trimws(as.character(val_sections$act))) > 0 &
    !is.na(val_sections$section) & nchar(trimws(as.character(val_sections$section))) > 0, , drop = FALSE
  ]

  val_clean$case_id <- as.character(val_clean$case_id)
  val_clean$identifier <- paste0(trimws(as.character(val_clean$act)), "::", trimws(as.character(val_clean$section)))
  val_unique <- unique(val_clean[, c("case_id", "identifier")])

  val_case_ids <- unique(val_unique$case_id)
  suppressWarnings(num_val <- as.numeric(val_case_ids))
  if (!any(is.na(num_val))) {
    val_case_ids <- val_case_ids[order(num_val)]
  } else {
    val_case_ids <- sort(val_case_ids)
  }

  n_val <- length(val_case_ids)

  val_scores <- stats::setNames(rep(NA_real_, n_val), val_case_ids)
  unseen_cases <- character(0)
  val_using_train_count <- 0L
  neighbour_map <- list()

  # 5. Project each validation case independently against training information
  for (v_id in val_case_ids) {
    case_secs <- val_unique$identifier[val_unique$case_id == v_id]

    # Detect unseen sections absent from training vocabulary
    unseen_secs <- setdiff(case_secs, train_sec_ids)
    if (length(unseen_secs) > 0) {
      unseen_cases <- c(unseen_cases, v_id)
    }

    seen_secs <- intersect(case_secs, train_sec_ids)

    if (length(seen_secs) == 0) {
      val_scores[v_id] <- NA_real_
      neighbour_map[[v_id]] <- data.frame(
        train_case_id = character(0),
        similarity = numeric(0),
        stringsAsFactors = FALSE
      )
      next
    }

    # Initial indicator vector over training vocabulary
    sec_idx <- match(seen_secs, train_sec_ids)
    x_init <- numeric(m_train)
    x_init[sec_idx] <- 1 / length(seen_secs)

    # One-step diffusion over training transition matrix
    if (alpha == 1) {
      x_diff <- x_init
    } else {
      x_init_sp <- Matrix::Matrix(x_init, nrow = 1, ncol = m_train, sparse = TRUE)
      x_diff_sp <- alpha * x_init_sp + (1 - alpha) * (x_init_sp %*% P_train)
      x_diff <- as.numeric(x_diff_sp)
      sum_diff <- sum(x_diff)
      if (sum_diff > 0) {
        x_diff <- x_diff / sum_diff
      }
    }

    # Apply training IDF weights
    w_v <- x_diff * idf_train
    l2_v <- sqrt(sum(w_v^2))

    if (l2_v == 0) {
      val_scores[v_id] <- NA_real_
      neighbour_map[[v_id]] <- data.frame(
        train_case_id = character(0),
        similarity = numeric(0),
        stringsAsFactors = FALSE
      )
      next
    }

    w_v_norm <- w_v / l2_v

    # Cosine similarity against training cases ONLY
    sims <- as.numeric(W_train_norm %*% w_v_norm)
    sims[sims < 0] <- 0
    sims[sims > 1] <- 1

    cand_idx <- which(sims > 0)

    if (length(cand_idx) == 0) {
      val_scores[v_id] <- NA_real_
      neighbour_map[[v_id]] <- data.frame(
        train_case_id = character(0),
        similarity = numeric(0),
        stringsAsFactors = FALSE
      )
      next
    }

    cand_sims <- sims[cand_idx]
    cand_train_ids <- train_case_ids[cand_idx]

    # Deterministic tie breaking: similarity descending, train_case_id ascending
    ord <- order(-cand_sims, cand_train_ids)
    selected_subidx <- ord[seq_len(min(k, length(ord)))]

    top_train_ids <- cand_train_ids[selected_subidx]
    top_sims <- cand_sims[selected_subidx]

    val_scores[v_id] <- mean(top_sims)
    val_using_train_count <- val_using_train_count + 1L

    neighbour_map[[v_id]] <- data.frame(
      train_case_id = top_train_ids,
      similarity = top_sims,
      stringsAsFactors = FALSE
    )
  }

  unseen_cases <- sort(unique(unseen_cases))

  list(
    validation_legal_scores = val_scores,
    unseen_section_cases = unseen_cases,
    val_cases_using_train_neighbours = val_using_train_count,
    val_cases_using_val_neighbours = 0L,
    neighbour_map = neighbour_map
  )
}

#' Temporal Validation of Delay Models
#'
#' Evaluates out-of-sample statistical discrimination (Harrell's C-index) and calibration
#' of a fitted \code{delay_model} using a chronological temporal holdout split based on filing dates.
#' Audits for potential future-neighbour leakage when predictors are constructed over a full-sample case graph,
#' and evaluates a strictly leakage-aware validation pipeline using training-only graph construction and validation projection.
#'
#' @param fit A \code{delay_model} object produced by \code{\link{fit_delay_model}}.
#' @param data Optional \code{data.frame} of cases for validation. Defaults to \code{fit$model_data}.
#' @param case_graph Optional \code{igraph} graph of case similarities. Defaults to \code{fit$case_graph}.
#' @param time_horizon Optional numeric time horizon (duration) for calibration evaluation. Defaults to median duration.
#' @param temporal_split Numeric fraction specifying the proportion of earliest filing date cases assigned to training. Defaults to \code{0.7}.
#' @param seed Optional integer random seed for reproducibility. Defaults to \code{123}.
#' @param sections Optional \code{data.frame} of legal section records used for leakage-aware validation projection.
#' @param k Positive integer specifying top-k neighbours for case graph. Defaults to \code{3}.
#' @param alpha Numeric diffusion weight in \code{[0, 1]}. Defaults to \code{0.7}.
#' @param use_idf Logical flag indicating whether to use IDF weighting. Defaults to \code{TRUE}.
#'
#' @return An S3 object of class \code{c("model_validation", "list")} containing:
#' \describe{
#'   \item{available}{Logical indicating whether temporal validation was successfully evaluated.}
#'   \item{temporal_split}{Numeric fraction used for splitting (e.g. 0.7).}
#'   \item{split_date}{\code{Date} cutoff threshold separating training and validation cases.}
#'   \item{n_train}{Number of training observations.}
#'   \item{n_val}{Number of validation observations.}
#'   \item{events_train}{Number of resolution events in the training subset.}
#'   \item{events_val}{Number of resolution events in the validation subset.}
#'   \item{train_date_range}{Vector of minimum and maximum filing dates in training.}
#'   \item{val_date_range}{Vector of minimum and maximum filing dates in validation.}
#'   \item{training_c_index}{Harrell's concordance index on the training set.}
#'   \item{validation_c_index}{Harrell's concordance index on the held-out validation set.}
#'   \item{naive_validation_c_index}{C-index computed using full-sample graph legal scores.}
#'   \item{leakage_aware_validation_c_index}{C-index computed using training-only graph construction and validation projection.}
#'   \item{future_neighbour_leakage}{Logical indicating whether full-sample training cases have future neighbours.}
#'   \item{val_cases_using_val_neighbours}{Count of validation cases using validation neighbours (0 by design).}
#'   \item{val_cases_using_train_neighbours}{Count of validation cases using training neighbours.}
#'   \item{unseen_section_val_cases}{Count of validation cases containing sections unseen in training.}
#'   \item{full_model_diagnostics}{List containing hazard ratios and \code{cox.zph} proportional hazards test.}
#'   \item{interpretation}{Explicit non-causal statistical safeguard statement.}
#'   \item{message}{Character description of validation status.}
#' }
#'
#' @examples
#' \dontrun{
#' val <- validate_models(fit, sections = sections_df, temporal_split = 0.7)
#' print(val)
#' }
#'
#' @export
validate_models <- function(fit,
                            data = NULL,
                            case_graph = NULL,
                            time_horizon = NULL,
                            temporal_split = 0.7,
                            seed = 123,
                            sections = NULL,
                            k = 3,
                            alpha = 0.7,
                            use_idf = TRUE) {
  if (!inherits(fit, "delay_model")) {
    stop("`fit` must be a `delay_model` object produced by `fit_delay_model()`.", call. = FALSE)
  }

  if (!is.numeric(temporal_split) || length(temporal_split) != 1 || temporal_split <= 0 || temporal_split >= 1) {
    stop("`temporal_split` must be a single numeric value strictly between 0 and 1.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  interp_str <- "Temporal validation evaluates how well the fitted model generalises to later-filed cases. Validation metrics describe predictive performance and do not establish causal relationships."

  if (!isTRUE(fit$fit_success) || is.null(fit$model)) {
    res <- list(
      available = FALSE,
      temporal_split = temporal_split,
      split_date = as.Date(NA),
      n_train = 0,
      n_val = 0,
      events_train = 0,
      events_val = 0,
      train_date_range = as.Date(c(NA, NA)),
      val_date_range = as.Date(c(NA, NA)),
      training_c_index = NA_real_,
      validation_c_index = NA_real_,
      naive_validation_c_index = NA_real_,
      leakage_aware_validation_c_index = NA_real_,
      calibration_available = FALSE,
      calibration_summary = data.frame(),
      future_neighbour_leakage = FALSE,
      n_training_cases_with_future_neighbours = 0L,
      n_future_neighbour_edges = 0L,
      max_future_neighbour_filing_date = as.Date(NA),
      clean_training_future_neighbour_edges = 0L,
      clean_training_future_neighbour_cases = 0L,
      val_cases_using_val_neighbours = 0L,
      val_cases_using_train_neighbours = 0L,
      unseen_section_val_cases = 0L,
      full_model_diagnostics = list(),
      interpretation = interp_str,
      message = sprintf("Validation unavailable: %s", ifelse(is.null(fit$fit_message), "Fitted Cox model did not converge.", fit$fit_message))
    )
    class(res) <- c("model_validation", "list")
    return(res)
  }

  v_data <- if (is.null(data)) fit$model_data else data
  if (!is.data.frame(v_data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  req_cols <- c("case_id", "filing_date", "duration", "event", "legal_score", "load_at_filing")
  missing_cols <- setdiff(req_cols, names(v_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required column(s) missing from validation data: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  v_mask <- !is.na(v_data$filing_date) &
             !is.na(v_data$duration) & v_data$duration > 0 &
             !is.na(v_data$event) & v_data$event %in% c(0, 1) &
             !is.na(v_data$legal_score) &
             !is.na(v_data$load_at_filing)

  v_data <- v_data[v_mask, , drop = FALSE]
  n_total <- nrow(v_data)

  if (n_total < 10) {
    res <- list(
      available = FALSE,
      temporal_split = temporal_split,
      split_date = as.Date(NA),
      n_train = n_total,
      n_val = 0,
      events_train = sum(v_data$event == 1),
      events_val = 0,
      train_date_range = as.Date(c(NA, NA)),
      val_date_range = as.Date(c(NA, NA)),
      training_c_index = NA_real_,
      validation_c_index = NA_real_,
      naive_validation_c_index = NA_real_,
      leakage_aware_validation_c_index = NA_real_,
      calibration_available = FALSE,
      calibration_summary = data.frame(),
      future_neighbour_leakage = FALSE,
      n_training_cases_with_future_neighbours = 0L,
      n_future_neighbour_edges = 0L,
      max_future_neighbour_filing_date = as.Date(NA),
      clean_training_future_neighbour_edges = 0L,
      clean_training_future_neighbour_cases = 0L,
      val_cases_using_val_neighbours = 0L,
      val_cases_using_train_neighbours = 0L,
      unseen_section_val_cases = 0L,
      full_model_diagnostics = list(hazard_ratios = fit$hazard_ratios, ph_test = fit$ph_test),
      interpretation = interp_str,
      message = sprintf("Validation unavailable: Insufficient observations (n = %d) for temporal split.", n_total)
    )
    class(res) <- c("model_validation", "list")
    return(res)
  }

  # Order strictly chronologically by filing_date (breaking ties deterministically by case_id)
  ord <- order(as.Date(v_data$filing_date), as.character(v_data$case_id))
  v_sorted <- v_data[ord, , drop = FALSE]
  rownames(v_sorted) <- NULL

  n_train <- floor(n_total * temporal_split)
  n_val <- n_total - n_train

  if (n_train < 5 || n_val < 5) {
    res <- list(
      available = FALSE,
      temporal_split = temporal_split,
      split_date = as.Date(NA),
      n_train = n_train,
      n_val = n_val,
      events_train = sum(v_sorted$event[1:n_train] == 1),
      events_val = sum(v_sorted$event[(n_train + 1):n_total] == 1),
      train_date_range = as.Date(c(NA, NA)),
      val_date_range = as.Date(c(NA, NA)),
      training_c_index = NA_real_,
      validation_c_index = NA_real_,
      naive_validation_c_index = NA_real_,
      leakage_aware_validation_c_index = NA_real_,
      calibration_available = FALSE,
      calibration_summary = data.frame(),
      future_neighbour_leakage = FALSE,
      n_training_cases_with_future_neighbours = 0L,
      n_future_neighbour_edges = 0L,
      max_future_neighbour_filing_date = as.Date(NA),
      clean_training_future_neighbour_edges = 0L,
      clean_training_future_neighbour_cases = 0L,
      val_cases_using_val_neighbours = 0L,
      val_cases_using_train_neighbours = 0L,
      unseen_section_val_cases = 0L,
      full_model_diagnostics = list(hazard_ratios = fit$hazard_ratios, ph_test = fit$ph_test),
      interpretation = interp_str,
      message = sprintf("Validation unavailable: Subset sizes too small (train: %d, val: %d).", n_train, n_val)
    )
    class(res) <- c("model_validation", "list")
    return(res)
  }

  train_df <- v_sorted[seq_len(n_train), , drop = FALSE]
  val_df <- v_sorted[(n_train + 1):n_total, , drop = FALSE]

  cutoff_date <- as.Date(train_df$filing_date[n_train])
  events_tr <- sum(train_df$event == 1)
  events_va <- sum(val_df$event == 1)

  if (events_tr == 0 || events_va == 0) {
    res <- list(
      available = FALSE,
      temporal_split = temporal_split,
      split_date = cutoff_date,
      n_train = n_train,
      n_val = n_val,
      events_train = events_tr,
      events_val = events_va,
      train_date_range = range(as.Date(train_df$filing_date)),
      val_date_range = range(as.Date(val_df$filing_date)),
      training_c_index = NA_real_,
      validation_c_index = NA_real_,
      naive_validation_c_index = NA_real_,
      leakage_aware_validation_c_index = NA_real_,
      calibration_available = FALSE,
      calibration_summary = data.frame(),
      future_neighbour_leakage = FALSE,
      n_training_cases_with_future_neighbours = 0L,
      n_future_neighbour_edges = 0L,
      max_future_neighbour_filing_date = as.Date(NA),
      clean_training_future_neighbour_edges = 0L,
      clean_training_future_neighbour_cases = 0L,
      val_cases_using_val_neighbours = 0L,
      val_cases_using_train_neighbours = 0L,
      unseen_section_val_cases = 0L,
      full_model_diagnostics = list(hazard_ratios = fit$hazard_ratios, ph_test = fit$ph_test),
      interpretation = interp_str,
      message = sprintf("Validation unavailable: Zero resolution events in %s.", if(events_tr == 0) "training set" else "validation set")
    )
    class(res) <- c("model_validation", "list")
    return(res)
  }

  # 1. Naive Future-Neighbour Leakage Audit on Full-Sample Graph
  cg <- if (!is.null(case_graph)) case_graph else fit$case_graph
  future_leakage <- FALSE
  n_train_with_future <- 0L
  n_future_edges <- 0L
  max_future_date <- as.Date(NA)

  if (inherits(cg, "igraph") && igraph::vcount(cg) > 0 && igraph::ecount(cg) > 0) {
    train_ids <- as.character(train_df$case_id)
    all_filing_dates <- stats::setNames(as.Date(v_sorted$filing_date), as.character(v_sorted$case_id))

    el <- igraph::as_data_frame(cg, what = "edges")
    if (nrow(el) > 0) {
      d_from <- all_filing_dates[as.character(el$from)]
      d_to <- all_filing_dates[as.character(el$to)]

      # Direction 1: from is train (<= cutoff_date) and to is future (> cutoff_date)
      u_tr_v_fut <- !is.na(d_from) & !is.na(d_to) & (d_from <= cutoff_date) & (d_to > cutoff_date)
      # Direction 2: to is train (<= cutoff_date) and from is future (> cutoff_date)
      v_tr_u_fut <- !is.na(d_from) & !is.na(d_to) & (d_to <= cutoff_date) & (d_from > cutoff_date)

      flagged_cases <- c(el$from[u_tr_v_fut], el$to[v_tr_u_fut])
      future_dates_collected <- c(d_to[u_tr_v_fut], d_from[v_tr_u_fut])

      n_train_with_future <- length(unique(flagged_cases))
      n_future_edges <- length(future_dates_collected)
      if (n_future_edges > 0) {
        future_leakage <- TRUE
        max_future_date <- max(future_dates_collected)
      }
    }
  }

  # Fit Naive Cox model ONLY on training subset using full-sample legal scores
  train_fit <- NULL
  fit_err <- NULL
  tryCatch({
    suppressWarnings({
      train_fit <- survival::coxph(
        survival::Surv(duration, event) ~ legal_score + load_at_filing,
        data = train_df
      )
    })
  }, error = function(e) {
    fit_err <<- e$message
  })

  if (is.null(train_fit) || !is.null(fit_err)) {
    res <- list(
      available = FALSE,
      temporal_split = temporal_split,
      split_date = cutoff_date,
      n_train = n_train,
      n_val = n_val,
      events_train = events_tr,
      events_val = events_va,
      train_date_range = range(as.Date(train_df$filing_date)),
      val_date_range = range(as.Date(val_df$filing_date)),
      training_c_index = NA_real_,
      validation_c_index = NA_real_,
      naive_validation_c_index = NA_real_,
      leakage_aware_validation_c_index = NA_real_,
      calibration_available = FALSE,
      calibration_summary = data.frame(),
      future_neighbour_leakage = future_leakage,
      n_training_cases_with_future_neighbours = n_train_with_future,
      n_future_neighbour_edges = n_future_edges,
      max_future_neighbour_filing_date = max_future_date,
      clean_training_future_neighbour_edges = 0L,
      clean_training_future_neighbour_cases = 0L,
      val_cases_using_val_neighbours = 0L,
      val_cases_using_train_neighbours = 0L,
      unseen_section_val_cases = 0L,
      full_model_diagnostics = list(hazard_ratios = fit$hazard_ratios, ph_test = fit$ph_test),
      interpretation = interp_str,
      message = sprintf("Validation unavailable: Training model fit failed (%s).", fit_err)
    )
    class(res) <- c("model_validation", "list")
    return(res)
  }

  # Harrell's Concordance Index for Naive Model
  tr_conc <- survival::concordance(train_fit)
  val_conc <- survival::concordance(train_fit, newdata = val_df)

  train_c_index <- unname(tr_conc$concordance)
  train_se <- sqrt(unname(tr_conc$var))

  naive_val_c_index <- unname(val_conc$concordance)
  val_se <- sqrt(unname(val_conc$var))

  # 2. Leakage-Aware Validation Architecture (if sections data provided)
  leakage_aware_val_c_index <- NA_real_
  clean_tr_future_edges <- 0L
  clean_tr_future_cases <- 0L
  val_using_val_nbrs <- 0L
  val_using_tr_nbrs <- 0L
  unseen_sec_val_cases <- 0L

  if (!is.null(sections) && is.data.frame(sections) && nrow(sections) > 0) {
    tryCatch({
      tr_cids <- as.character(train_df$case_id)
      val_cids <- as.character(val_df$case_id)

      tr_sec_df <- sections[as.character(sections$case_id) %in% tr_cids, , drop = FALSE]
      val_sec_df <- sections[as.character(sections$case_id) %in% val_cids, , drop = FALSE]

      if (nrow(tr_sec_df) > 0 && nrow(val_sec_df) > 0) {
        # Build training section graph and training case graph from training cases ONLY
        tr_sec_graph <- build_section_graph(tr_sec_df)
        tr_diff_p <- diffuse_profiles(tr_sec_df, tr_sec_graph, alpha = alpha)
        tr_case_graph <- build_case_graph(tr_diff_p, k = k, use_idf = use_idf)

        # Audit training graph for zero future-neighbour leakage
        tr_cases_clean <- train_df[, c("case_id", "filing_date"), drop = FALSE]
        tr_leak_audit <- check_future_neighbours(tr_case_graph, tr_cases_clean, cutoff_date = cutoff_date)
        clean_tr_future_edges <- tr_leak_audit$n_future_neighbour_edges
        clean_tr_future_cases <- tr_leak_audit$n_cases_with_future_neighbours

        # Compute training legal scores strictly from training graph
        tr_legal_scores <- rep(NA_real_, nrow(train_df))
        if (igraph::vcount(tr_case_graph) > 0) {
          tr_v_names <- igraph::V(tr_case_graph)$name
          if (!is.null(tr_v_names)) {
            tr_w <- if ("weight" %in% igraph::edge_attr_names(tr_case_graph)) igraph::E(tr_case_graph)$weight else NULL
            tr_deg <- igraph::degree(tr_case_graph, mode = "all")
            tr_str <- if (!is.null(tr_w)) igraph::strength(tr_case_graph, mode = "all", weights = tr_w) else as.numeric(tr_deg)
            tr_v_scores <- ifelse(tr_deg > 0, tr_str / tr_deg, NA_real_)
            tr_map <- match(tr_cids, tr_v_names)
            tr_has <- !is.na(tr_map)
            tr_legal_scores[tr_has] <- tr_v_scores[tr_map[tr_has]]
          }
        }

        train_df_leakfree <- train_df
        train_df_leakfree$legal_score <- tr_legal_scores
        train_df_leakfree <- train_df_leakfree[!is.na(train_df_leakfree$legal_score), , drop = FALSE]

        if (nrow(train_df_leakfree) >= 5 && sum(train_df_leakfree$event == 1) > 0) {
          # Fit Cox model on training cases with training-only predictors
          cox_tr_leakfree <- survival::coxph(
            survival::Surv(duration, event) ~ legal_score + load_at_filing,
            data = train_df_leakfree
          )

          # Project validation cases onto training legal section representation space
          proj_res <- project_validation_legal_score(
            val_sections = val_sec_df,
            train_sections = tr_sec_df,
            k = k,
            use_idf = use_idf,
            alpha = alpha
          )

          val_df_leakfree <- val_df
          val_df_leakfree$legal_score <- proj_res$validation_legal_scores[as.character(val_df_leakfree$case_id)]
          val_df_leakfree <- val_df_leakfree[!is.na(val_df_leakfree$legal_score), , drop = FALSE]

          if (nrow(val_df_leakfree) >= 1 && sum(val_df_leakfree$event == 1) > 0) {
            val_conc_leakfree <- survival::concordance(cox_tr_leakfree, newdata = val_df_leakfree)
            leakage_aware_val_c_index <- unname(val_conc_leakfree$concordance)
          }

          val_using_val_nbrs <- proj_res$val_cases_using_val_neighbours
          val_using_tr_nbrs <- proj_res$val_cases_using_train_neighbours
          unseen_sec_val_cases <- length(proj_res$unseen_section_cases)
        }
      }
    }, error = function(e) {
      leakage_aware_val_c_index <<- NA_real_
    })
  }

  # Calibration evaluation at time_horizon
  if (is.null(time_horizon)) {
    time_horizon <- stats::median(val_df$duration[val_df$event == 1])
    if (is.na(time_horizon) || time_horizon <= 0) {
      time_horizon <- stats::median(val_df$duration)
    }
  }

  calib_available <- FALSE
  calib_df <- data.frame()
  tryCatch({
    sf <- survival::survfit(train_fit, newdata = val_df)
    s_sum <- summary(sf, times = time_horizon)
    if (!is.null(s_sum$surv) && length(s_sum$surv) > 0) {
      pred_surv <- mean(s_sum$surv, na.rm = TRUE)
      km_val <- survival::survfit(survival::Surv(duration, event) ~ 1, data = val_df)
      km_sum <- summary(km_val, times = time_horizon)
      obs_surv <- if (length(km_sum$surv) > 0) km_sum$surv[1] else NA_real_

      calib_df <- data.frame(
        time_horizon = time_horizon,
        mean_predicted_survival = pred_surv,
        observed_km_survival = obs_surv,
        prediction_error = pred_surv - obs_surv,
        stringsAsFactors = FALSE
      )
      calib_available <- !is.na(pred_surv) && !is.na(obs_surv)
    }
  }, error = function(e) {
    calib_available <<- FALSE
  })

  final_val_c_index <- if (!is.na(leakage_aware_val_c_index)) leakage_aware_val_c_index else naive_val_c_index

  res <- list(
    available = TRUE,
    temporal_split = temporal_split,
    split_date = cutoff_date,
    n_train = n_train,
    n_val = n_val,
    events_train = events_tr,
    events_val = events_va,
    train_date_range = range(as.Date(train_df$filing_date)),
    val_date_range = range(as.Date(val_df$filing_date)),
    training_c_index = train_c_index,
    training_se = train_se,
    validation_c_index = final_val_c_index,
    validation_se = val_se,
    naive_validation_c_index = naive_val_c_index,
    leakage_aware_validation_c_index = leakage_aware_val_c_index,
    calibration_available = calib_available,
    calibration_summary = calib_df,
    future_neighbour_leakage = future_leakage,
    n_training_cases_with_future_neighbours = n_train_with_future,
    n_future_neighbour_edges = n_future_edges,
    max_future_neighbour_filing_date = max_future_date,
    clean_training_future_neighbour_edges = clean_tr_future_edges,
    clean_training_future_neighbour_cases = clean_tr_future_cases,
    val_cases_using_val_neighbours = val_using_val_nbrs,
    val_cases_using_train_neighbours = val_using_tr_nbrs,
    unseen_section_val_cases = unseen_sec_val_cases,
    full_model_diagnostics = list(
      hazard_ratios = fit$hazard_ratios,
      ph_test = fit$ph_test
    ),
    interpretation = interp_str,
    message = "Temporal validation completed successfully."
  )
  class(res) <- c("model_validation", "list")
  res
}

#' Print method for model_validation
#' @param x A \code{model_validation} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print model_validation
print.model_validation <- function(x, ...) {
  cat("Docketscape Delay Model Temporal Validation\n")
  cat("===========================================\n")
  cat("Status:     ", x$message, "\n")
  cat("Available:  ", x$available, "\n\n")

  if (x$available) {
    if (!is.na(x$leakage_aware_validation_c_index)) {
      cat("==================================================\n")
      cat("LEAKAGE-AWARE TEMPORAL VALIDATION\n")
      cat("==================================================\n\n")
      cat("Training cases:                         ", x$n_train, "\n", sep = "")
      cat("Validation cases:                       ", x$n_val, "\n\n", sep = "")
      cat("Training future-neighbour edges:        ", x$clean_training_future_neighbour_edges, "\n", sep = "")
      cat("Training future-neighbour cases:        ", x$clean_training_future_neighbour_cases, "\n\n", sep = "")
      cat("Naive validation C-index:               ", sprintf("%.4f", x$naive_validation_c_index), "\n", sep = "")
      cat("Leakage-aware validation C-index:       ", sprintf("%.4f", x$leakage_aware_validation_c_index), "\n\n", sep = "")
      cat("Validation cases using validation neighbours: ", x$val_cases_using_val_neighbours, "\n", sep = "")
      cat("Validation cases using training neighbours:   ", x$val_cases_using_train_neighbours, "\n\n", sep = "")
      cat("Unseen-section validation cases:       ", x$unseen_section_val_cases, "\n\n", sep = "")
    } else {
      cat("Temporal Holdout Split Summary:\n")
      cat("  - Split fraction:  ", round(x$temporal_split * 100, 1), "% training / ", round((1 - x$temporal_split) * 100, 1), "% validation\n", sep = "")
      cat("  - Temporal cutoff: ", as.character(x$split_date), "\n")
      cat("  - Training:        ", x$n_train, " cases (", x$events_train, " events), filing dates ", as.character(x$train_date_range[1]), " to ", as.character(x$train_date_range[2]), "\n", sep = "")
      cat("  - Validation:      ", x$n_val, " cases (", x$events_val, " events), filing dates ", as.character(x$val_date_range[1]), " to ", as.character(x$val_date_range[2]), "\n\n", sep = "")

      cat("Future-Neighbour Leakage Audit:\n")
      cat("  - Future-neighbour leakage detected: ", x$future_neighbour_leakage, "\n", sep = "")
      cat("  - Training cases with future neighbours: ", x$n_training_cases_with_future_neighbours, "\n", sep = "")
      cat("  - Total future-neighbour edges:          ", x$n_future_neighbour_edges, "\n", sep = "")
      if (x$future_neighbour_leakage) {
        cat("  - Latest future-neighbour filing date:   ", as.character(x$max_future_neighbour_filing_date), "\n", sep = "")
      }
      cat("\n")

      cat("Harrell's Concordance Index (C-Index):\n")
      cat("  - Training C-Index:   ", sprintf("%.4f (SE: %.4f)", x$training_c_index, x$training_se), "\n")
      cat("  - Naive Validation C-Index: ", sprintf("%.4f (SE: %.4f)", x$naive_validation_c_index, x$validation_se), "\n\n")

      if (x$calibration_available && nrow(x$calibration_summary) > 0) {
        cat("Time-Horizon Calibration Summary:\n")
        print(x$calibration_summary)
        cat("\n")
      } else {
        cat("Calibration: Not evaluated or insufficient validation sample size.\n\n")
      }
    }
  }

  cat("Statistical Safeguard:\n")
  cat(x$interpretation, "\n")
  invisible(x)
}
