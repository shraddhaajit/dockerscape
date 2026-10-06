#' Decompose Delay Model Linear Predictors
#'
#' Decomposes the log-hazard linear predictor of a fitted \code{delay_model} into
#' legal-similarity and filing-time docket-load association components.
#'
#' @param fit A \code{delay_model} object produced by \code{\link{fit_delay_model}}.
#'
#' @return An S3 object of class \code{c("delay_decomposition", "list")} containing:
#' \describe{
#'   \item{case_decomposition}{A \code{data.frame} of case-level predictor components and absolute shares.}
#'   \item{summary}{A \code{data.frame} of aggregate summary metrics across all decomposed cases.}
#'   \item{coefficients}{A named vector of the fitted Cox model coefficients (\eqn{\beta_{\text{legal}}, \beta_{\text{load}}}).}
#'   \item{model}{The underlying fitted \code{\link[survival]{coxph}} model object.}
#'   \item{interpretation}{Explicit non-causal statistical safeguard statement.}
#'   \item{available}{Logical indicating whether decomposition was successfully computed.}
#'   \item{message}{Status message describing decomposition availability.}
#' }
#'
#' @details
#' \bold{Linear Predictor Decomposition:}
#' For each included case \eqn{i}, the fitted Cox model log-hazard linear predictor \eqn{\eta_i} is decomposed as:
#' \deqn{\eta_i = \beta_{\text{legal}} \cdot \text{legal\_score}_i + \beta_{\text{load}} \cdot \text{load\_at\_filing}_i}
#' \itemize{
#'   \item \bold{Legal Component:} \eqn{\text{legal\_component}_i = \beta_{\text{legal}} \cdot \text{legal\_score}_i}
#'   \item \bold{Load Component:} \eqn{\text{load\_component}_i = \beta_{\text{load}} \cdot \text{load\_at\_filing}_i}
#'   \item \bold{Total Linear Predictor:} \eqn{\text{total\_linear\_predictor}_i = \text{legal\_component}_i + \text{load\_component}_i}
#' }
#'
#' \bold{Absolute Shares:}
#' Relative contribution shares are calculated on absolute component magnitudes to prevent denominator distortion or sign cancellation:
#' \deqn{\text{absolute\_legal\_share}_i = \frac{|\text{legal\_component}_i|}{|\text{legal\_component}_i| + |\text{load\_component}_i|}}
#' \deqn{\text{absolute\_load\_share}_i = \frac{|\text{load\_component}_i|}{|\text{legal\_component}_i| + |\text{load\_component}_i|}}
#' If both components are exactly zero, both absolute shares return \code{NA}.
#'
#' \bold{Statistical Safeguard:}
#' The decomposition partitions the fitted Cox model's linear predictor into observational associations with legal similarity
#' and filing-time docket load. It does NOT estimate causal contributions to delay, judge effects, or court performance/efficiency scores.
#'
#' @examples
#' \dontrun{
#' fit <- fit_delay_model(cleaned_cases, case_graph, court_load_df)
#' decomp <- decompose_delay(fit)
#' print(decomp)
#' }
#'
#' @export
decompose_delay <- function(fit) {
  if (!inherits(fit, "delay_model")) {
    stop("`fit` must be a `delay_model` object produced by `fit_delay_model()`.", call. = FALSE)
  }

  interp_str <- "The decomposition partitions the fitted Cox model's linear predictor into associations associated with legal similarity and filing-time docket load. It does not estimate causal contributions to delay."

  # Check if model fitting converged and is valid
  if (!isTRUE(fit$fit_success) || is.null(fit$model)) {
    res <- list(
      case_decomposition = data.frame(
        case_id = character(0),
        legal_score = numeric(0),
        load_at_filing = numeric(0),
        legal_component = numeric(0),
        load_component = numeric(0),
        total_linear_predictor = numeric(0),
        absolute_legal_share = numeric(0),
        absolute_load_share = numeric(0),
        stringsAsFactors = FALSE
      ),
      summary = data.frame(),
      coefficients = numeric(0),
      model = NULL,
      interpretation = interp_str,
      available = FALSE,
      message = sprintf("Decomposition unavailable: %s", ifelse(is.null(fit$fit_message), "Model not fitted or converged.", fit$fit_message))
    )
    class(res) <- c("delay_decomposition", "list")
    return(res)
  }

  model_data <- fit$model_data
  req_cols <- c("case_id", "legal_score", "load_at_filing", "duration", "event")
  missing_cols <- setdiff(req_cols, names(model_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required column(s) missing from `fit$model_data`: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  b_vec <- stats::coef(fit$model)
  beta_legal <- unname(b_vec["legal_score"])
  beta_load <- unname(b_vec["load_at_filing"])

  if (is.na(beta_legal) || is.na(beta_load)) {
    res <- list(
      case_decomposition = data.frame(),
      summary = data.frame(),
      coefficients = numeric(0),
      model = fit$model,
      interpretation = interp_str,
      available = FALSE,
      message = "Decomposition unavailable: Cox model coefficient estimates contain NA."
    )
    class(res) <- c("delay_decomposition", "list")
    return(res)
  }

  legal_comp <- beta_legal * model_data$legal_score
  load_comp <- beta_load * model_data$load_at_filing
  total_lp <- legal_comp + load_comp

  abs_sum <- abs(legal_comp) + abs(load_comp)
  abs_legal_share <- ifelse(abs_sum == 0, NA_real_, abs(legal_comp) / abs_sum)
  abs_load_share <- ifelse(abs_sum == 0, NA_real_, abs(load_comp) / abs_sum)

  case_decomp <- data.frame(
    case_id = model_data$case_id,
    legal_score = model_data$legal_score,
    load_at_filing = model_data$load_at_filing,
    legal_component = legal_comp,
    load_component = load_comp,
    total_linear_predictor = total_lp,
    absolute_legal_share = abs_legal_share,
    absolute_load_share = abs_load_share,
    stringsAsFactors = FALSE
  )

  sum_df <- data.frame(
    n_cases = nrow(case_decomp),
    mean_legal_component = mean(case_decomp$legal_component, na.rm = TRUE),
    mean_load_component = mean(case_decomp$load_component, na.rm = TRUE),
    median_legal_component = stats::median(case_decomp$legal_component, na.rm = TRUE),
    median_load_component = stats::median(case_decomp$load_component, na.rm = TRUE),
    mean_abs_legal_component = mean(abs(case_decomp$legal_component), na.rm = TRUE),
    mean_abs_load_component = mean(abs(case_decomp$load_component), na.rm = TRUE),
    mean_abs_legal_share = mean(case_decomp$absolute_legal_share, na.rm = TRUE),
    mean_abs_load_share = mean(case_decomp$absolute_load_share, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  res <- list(
    case_decomposition = case_decomp,
    summary = sum_df,
    coefficients = c(legal_score = beta_legal, load_at_filing = beta_load),
    model = fit$model,
    interpretation = interp_str,
    available = TRUE,
    message = "Delay decomposition computed successfully."
  )
  class(res) <- c("delay_decomposition", "list")
  res
}

#' Print method for delay_decomposition
#' @param x A \code{delay_decomposition} object.
#' @param ... Additional arguments (unused).
#' @export
#' @method print delay_decomposition
print.delay_decomposition <- function(x, ...) {
  cat("Docketscape Delay Linear Predictor Decomposition\n")
  cat("================================================\n")
  cat("Status:      ", x$message, "\n")
  cat("Available:   ", x$available, "\n\n")

  if (x$available && nrow(x$case_decomposition) > 0) {
    cat("Model Coefficients:\n")
    print(x$coefficients)
    cat("\nAggregate Decomposition Summary:\n")
    print(x$summary)
    cat("\nHead of Case Decomposition Table:\n")
    print(head(x$case_decomposition, 6))
    cat("\n")
  }

  cat("Statistical Safeguard:\n")
  cat(x$interpretation, "\n")
  invisible(x)
}
