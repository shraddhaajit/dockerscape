# Helper function to generate Phase 5 synthetic dataset

get_synthetic_cases <- function() {
  data.frame(
    case_id = 1:6,
    court = c("C1", "C1", "C1", "C2", "C2", "C2"),
    case_type = c("Cheque", "Cheque", "Criminal", "Cheque", "Criminal", "Criminal"),
    filing_date = as.Date(c(
      "2012-01-10",
      "2012-03-05",
      "2012-06-20",
      "2013-02-11",
      "2013-05-30",
      "2014-01-15"
    )),
    decision_date = as.Date(c(
      "2012-11-02",
      "2013-02-14",
      NA,
      "2013-12-01",
      NA,
      NA
    )),
    stringsAsFactors = FALSE
  )
}

get_synthetic_sections <- function() {
  data.frame(
    case_id = c(1, 1, 2, 2, 3, 4, 4, 5, 6),
    act = c(
      "NI Act", "NI Act",
      "NI Act", "NI Act",
      "IPC",
      "NI Act", "NI Act",
      "IPC",
      "IPC"
    ),
    section = c(
      "138", "141",
      "138", "142",
      "302",
      "138", "141",
      "420",
      "302"
    ),
    stringsAsFactors = FALSE
  )
}
