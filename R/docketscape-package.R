#' docketscape: Legal Network and Congestion-Aware Delay Modeling for Court Dockets
#'
#' Splits court delay into substantive legal complexity (represented by statutory
#' networks of Acts and Sections) and institutional congestion (represented by
#' monthly court docket load).
#'
#' @references
#' Development Data Lab (DDL) Judiciary Dataset: \url{https://www.devdatalab.org/judicial-data}
#'
#' Asher, S., Novosad, P., et al. (2021). "In-Group Bias in the Indian Judiciary: Evidence from 5.5 Million Criminal Cases." Development Data Lab.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import survival
#' @import igraph
#' @import Matrix
#' @importFrom stats aggregate coef median predict quantile sd setNames
## usethis namespace: end
NULL
