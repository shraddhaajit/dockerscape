# docketscape: Legal Network and Congestion-Aware Delay Modeling for Court Dockets

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`docketscape` is an R package for case-level judicial records (such as the Development Data Lab extract of Indian lower-court records). It decomposes court delay into two distinct dimensions:

1. **What a case is** — represented through the statutory Acts and Sections invoked by the case.
2. **Where the case sits** — represented by the time-varying court docket load.

The package constructs a weighted similarity graph of cases based on shared and diffused legal provisions, estimates censoring-aware expected resolution timelines using legally similar neighbours, calculates relative case lateness, and models resolution hazard relative to filing-time court docket load.

---

## Methodological Boundaries and Disclaimers

`docketscape` is strictly an observational, representation-based modeling framework. To prevent misuse, the package enforces the following boundaries:

- **NOT a judge ranking system**: The package does not rank judges, courts, or judicial officers.
- **NOT a judicial performance score**: Relative lateness measures elapsed time relative to legally similar cases; it does not measure diligence, competence, or quality of adjudication.
- **NOT a causal congestion estimator**: Delay models estimate observational associations between baseline load and resolution hazard, not causal impacts of congestion or court administration.
- **NOT an outcome prediction engine**: The package models case durations, not judicial merits, verdicts, or decision outcomes.
- **NOT a mishandling detector**: Being slower than historical neighbours does not imply that a case was mishandled or neglected.

---

## Overview

### The Problem of Judicial Delay Analysis
Empirical analysis of court delay faces two persistent methodological challenges:

1. **Legal Heterogeneity ("What a case is"):** Cases differ fundamentally in procedural complexity, evidentiary burdens, and statutory scope. Treating all cases within broad case types as homogeneous obscures statutory differences, while analyzing raw section codes leads to severe sparsity.
2. **Institutional Congestion ("Where it sits"):** Case progress is mediated by the queue of pending matters in the assigned court. A case filed during peak docket backlog experiences systemic friction independent of its substantive legal complexity.

### Key Methodological Components

- **Right Censoring Matters:** At any observation date, open (pending) cases have not yet reached disposition. Treating pending cases as missing data or ignoring them introduces severe survivor bias (conditioning on early completion). `docketscape` treats pending matters as right-censored survival observations with event indicator $0$ and duration equal to follow-up from filing to the observation cutoff.
- **Legal Similarity via Network Diffusion:** Legal sections rarely operate in isolation. By constructing a bipartite case-section network, projecting it to a section co-occurrence graph, and diffusing case profiles over co-cited sections via one-step weighted profile diffusion ($X_{\text{raw}} = \alpha X_{\text{init}} + (1-\alpha) X_{\text{init}} P$), `docketscape` captures substantive affinities between provisions that co-occur across cases.
- **Neighbourhood Survival Experience:** For any focal case, an expected timeline is constructed from its $k$-nearest legal neighbours using weighted Kaplan-Meier estimation. Censored neighbours remain in the risk set until their last observation, properly anchoring historical benchmarks.
- **Filing-Time Court Load:** Docket backlog is quantified as open case volume at the calendar month of filing, establishing baseline institutional congestion at case inception.

---

## Installation

Install the package directly from GitHub using the repository URL:

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("shraddhaajit/dockerscape")
```

Or install locally for development:

```r
# Install from local source directory
devtools::install(".")
```

---

## Core Workflow

The package pipeline follows a modular sequence:

```text
clean_cases()
    ↓
build_section_graph()
    ↓
diffuse_profiles()
    ↓
build_case_graph()
    ↓
neighbourhood_survival()
    ↓
relative_lateness()
    ↓
court_load()
    ↓
fit_delay_model()
    ↓
decompose_delay()
    ↓
validate_models()
```

Auditing functions inspect data integrity and graph validity:

```text
check_future_neighbours()   # Detects edges from past cases to future cases
check_court_overlap()       # Quantifies shared vs unseen courts across splits
check_near_duplicates()     # Identifies same-court/date clusters and duplicates
```

### Temporal Validation and Leakage-Aware Projection

To evaluate out-of-sample performance without future information leakage, `docketscape` implements a strictly separated temporal projection workflow:

```text
TRAINING DATA (Earliest Filing Dates)
    ↓
build_section_graph(train_sections)
    ↓
diffuse_profiles(train_sections, train_section_graph)
    ↓
Compute Training IDF weights
    ↓
build_case_graph(train_diffused)
    ↓
fit_delay_model(train_cases, train_case_graph, train_court_load)

VALIDATION CASES (Later Filing Dates)
    ↓
project_validation_legal_score(val_sections, train_sections)
    - Projected into training section vocabulary and transition space
    - Connects ONLY to training cases as neighbours
    - Validation cases NEVER form neighbours with each other
    - Training graph, vocabulary, and IDF are NEVER modified
    ↓
Evaluate out-of-sample Harrell's C-index
```

---

## Quick Start: End-to-End Synthetic Pipeline

The following self-contained example runs the entire workflow from case cleaning to model validation:

```r
library(docketscape)

# 1. Simulate synthetic court cases
set.seed(123)
n_cases <- 80
cases <- data.frame(
  case_id = 1:n_cases,
  court = sample(c("Court_A", "Court_B", "Court_C"), n_cases, replace = TRUE),
  filing_date = sample(seq.Date(as.Date("2012-01-01"), as.Date("2013-06-01"), by = "month"), n_cases, replace = TRUE),
  duration_days = round(runif(n_cases, 40, 600)),
  is_disposed = rbinom(n_cases, 1, 0.75),
  stringsAsFactors = FALSE
)

# Populate decision date for disposed cases; pending cases have NA
cases$decision_date <- as.Date(NA)
cases$decision_date[cases$is_disposed == 1] <- 
  cases$filing_date[cases$is_disposed == 1] + cases$duration_days[cases$is_disposed == 1]

# 2. Clean cases and establish survival status relative to an observation cutoff
cleaned <- clean_cases(cases, observation_end = "2015-01-01")

# 3. Simulate legal sections invoked per case
set.seed(42)
acts_pool <- c("NI Act", "IPC", "Customs", "CrPC", "Companies Act")
sections_pool <- list(
  "NI Act" = c("138", "141", "142"),
  "IPC" = c("302", "307", "420", "406"),
  "Customs" = c("110", "112", "135"),
  "CrPC" = c("154", "482", "167"),
  "Companies Act" = c("241", "242", "447")
)

sec_list <- lapply(1:n_cases, function(i) {
  n_sec <- sample(2:4, 1)
  acts <- sample(acts_pool, n_sec, replace = TRUE)
  secs <- sapply(acts, function(a) sample(sections_pool[[a]], 1))
  data.frame(case_id = i, act = unname(acts), section = unname(secs), stringsAsFactors = FALSE)
})
sections_df <- unique(do.call(rbind, sec_list))

# 4. Construct section co-occurrence graph and diffuse legal profiles
sec_graph <- build_section_graph(sections_df)
diffused <- diffuse_profiles(sections_df, sec_graph, alpha = 0.7)

# 5. Build k-nearest-neighbour case similarity graph
case_graph <- build_case_graph(diffused, k = 5, use_idf = TRUE)

# 6. Estimate neighbourhood survival and relative lateness for a focal case
km_focal <- neighbourhood_survival(case_graph, cleaned, case_id = 1)
lateness_focal <- relative_lateness(km_focal)
print(lateness_focal)

# 7. Compute monthly court docket load
load_df <- court_load(cleaned, by = "month")

# 8. Fit delay Cox proportional hazards model
fit <- fit_delay_model(cleaned, case_graph, load_df)
print(fit)

# 9. Decompose delay linear predictor into legal and load components
decomp <- decompose_delay(fit)
print(head(decomp$case_decomposition, 3))

# 10. Perform leakage-aware temporal holdout validation
val_results <- validate_models(fit, sections = sections_df, temporal_split = 0.7)
print(val_results)
```

---

## Six-Case Demonstration

To illustrate core data mechanics, consider a compact 6-case scenario:

```r
cases_six <- data.frame(
  case_id = 1:6,
  court = c("C1", "C1", "C1", "C2", "C2", "C2"),
  case_type = c("Cheque", "Cheque", "Criminal", "Cheque", "Criminal", "Criminal"),
  filing_date = as.Date(c(
    "2012-01-10", "2012-03-05", "2012-06-20",
    "2013-02-11", "2013-05-30", "2014-01-15"
  )),
  decision_date = as.Date(c(
    "2012-11-02", "2013-02-14", NA,
    "2013-12-01", NA, NA
  ))
)

cleaned_six <- clean_cases(cases_six, observation_end = "2014-12-31")
print(cleaned_six[, c("case_id", "court", "duration", "event", "is_valid")])
```

Output:
```text
  case_id court duration event is_valid
1       1    C1      297     1     TRUE
2       2    C1      346     1     TRUE
3       3    C1      924     0     TRUE
4       4    C2      293     1     TRUE
5       5    C2      580     0     TRUE
6       6    C2      350     0     TRUE
```

### Key Statistical Points from Small-Sample Behaviour:
- **Pending cases are right-censored:** Cases 3, 5, and 6 have `event = 0` and durations measured from filing to the observation cutoff (`2014-12-31`). They are retained in the analysis rather than discarded.
- **Cox model convergence depends on sample variation:** In small sample sets with sparse events, semi-parametric Cox regression can fail to converge (e.g. running out of Newton-Raphson iterations or encountering rank deficiency).
- **Graceful handling without fabrication:** If numerical optimization cannot converge, `docketscape` sets `fit_success = FALSE` and records the exact numerical diagnostics in `fit_message`. Downstream functions (`decompose_delay()`, `validate_models()`) return explicit unavailability flags (`available = FALSE`) rather than fabricating non-converged statistics.

---

## Methodology

### 5.1 Case Cleaning (`clean_cases`)
- **Disposed vs. Pending:** Disposed cases have an observed `decision_date`, yielding `event = 1` and `duration = decision_date - filing_date`. Pending cases have missing `decision_date`, yielding `event = 0` (right-censored) and `duration = observation_end - filing_date`.
- **Validation Rules:** Records are flagged as invalid (`is_valid = FALSE`) with explicit reasons if:
  - Filing date or decision date is missing or unparseable.
  - Decision date precedes filing date (`decision_before_filing`).
  - Filing date is strictly after `observation_end` (`filed_after_observation_end`).
  - Duration is non-positive (`non_positive_duration`).
- Invalid records are flagged rather than silently dropped, maintaining auditability.

### 5.2 Section Graph (`build_section_graph`)
- **Nodes:** Statutory provisions defined by composite keys `Act::Section` (e.g., `IPC::420`).
- **Edges:** Undirected edges between sections that co-occur within the same case record.
- **Weights:** Edge weight $W_{jk}$ equals the count of distinct cases citing both section $j$ and section $k$.

### 5.3 Profile Diffusion (`diffuse_profiles`)
Raw citation vectors are sparse. A case citing Section 420 (cheating) shares substantive affinity with Section 406 (criminal breach of trust) if those sections frequently co-occur across the court's docket.

The package computes a one-step weighted profile diffusion:

$$X_{\text{raw}} = \alpha X_{\text{init}} + (1 - \alpha) X_{\text{init}} P$$

where $P = D^{-1} A$ is the row-stochastic section transition matrix.
- $\alpha \in [0, 1]$ governs retention of directly cited sections versus network smoothing.
- This is an exact one-step convex combination smoothing operation rather than multi-step power iteration or full stationary PageRank.
- Rows of $X_{\text{raw}}$ are subsequently $L_1$-normalized so each case's legal profile sums to 1.

### 5.4 Case Similarity (`build_case_graph`)
- **Inverse Document Frequency (IDF):** Rare statutory provisions carry higher discriminative information than ubiquitous sections:
  $$\text{IDF}_s = \log\left(\frac{N}{\text{DF}_s}\right) + 1$$
- **Cosine Similarity:** Similarity between cases $i$ and $j$ is evaluated on IDF-weighted diffused vectors:
  $$\text{sim}(i, j) = \frac{x_i W_{\text{IDF}} x_j^T}{\|x_i W_{\text{IDF}}\|_2 \, \|x_j W_{\text{IDF}}\|_2}$$
- **$k$-Nearest Neighbours:** Each case identifies its top $k$ most similar peers. Ties are broken deterministically by peer `case_id`.
- **Symmetrization:** Directed $k$-NN edges are symmetrized to an undirected graph. Mutual neighbours take the maximum similarity weight, and self-loops are eliminated.

### 5.5 Neighbourhood Survival (`neighbourhood_survival`)
For a target case $i$, an expected resolution timeline is estimated from its valid neighbours in the case graph:
- A weighted Kaplan-Meier estimator is fitted over the neighbours, where case weights equal the graph similarity edge weights $w_{ij}$.
- **Censoring Handling:** Censored neighbours contribute fully to the risk set $Y(t)$ up to their censoring durations, correctly preventing underestimation of resolution times.
- If all neighbours are right-censored, the survival curve does not drop to zero, and the median survival time remains undefined (`NA`).

### 5.6 Relative Lateness (`relative_lateness`)
Relative lateness evaluates where a target case falls within its peer survival curve:

$$\text{relative\_position} = 1 - S(T)$$

where $T$ is the target case duration, and $S(T)$ is the weighted Kaplan-Meier neighbourhood survival function evaluated at $T$.
- **Estimability:** If target duration $T$ exceeds the maximum follow-up observed among its neighbours, the package sets `estimable = FALSE` and returns `NA`. `docketscape` explicitly refuses to extrapolate beyond empirical neighbour follow-up.

### 5.7 Court Load (`court_load`)
- **Monthly Alignment:** Open docket load is evaluated at monthly resolution. Each case's filing date is mapped to the first day of its calendar month (`format(filing_date, "%Y-%m-01")`).
- **Load Definition:** `load_at_filing` is the count of active (open) cases in that court as of the filing month. It reflects baseline institutional queue pressure at entry, not cumulative lifetime congestion.

### 5.8 Cox Delay Model (`fit_delay_model`)
Case resolution hazard is modeled using semi-parametric Cox proportional hazards regression:

$$\lambda(t \mid X) = \lambda_0(t) \exp\left(\beta_{\text{legal}} \cdot \text{legal\_score}_i + \beta_{\text{load}} \cdot \text{load\_at\_filing}_i\right)$$

- `legal_score`: Mean similarity weight of incident edges in the case similarity graph.
- `load_at_filing`: Court docket load at the case's filing month.
- **Hazard Ratio Interpretation:** A hazard ratio $\exp(\beta) > 1$ denotes a higher hazard of resolution (faster disposition rate), whereas $\exp(\beta) < 1$ denotes a lower hazard of resolution (longer duration).
- **Observational Status:** Coefficients represent observational associations conditional on the model specification. They do not represent causal mechanisms.

### 5.9 Delay Decomposition (`decompose_delay`)
The fitted linear predictor $\eta_i$ is partitioned into its linear additive components:

$$\eta_i = \beta_{\text{legal}} \cdot \text{legal\_score}_i + \beta_{\text{load}} \cdot \text{load\_at\_filing}_i$$

Component shares are computed using absolute magnitudes:

$$\text{absolute\_legal\_share}_i = \frac{|\beta_{\text{legal}} \cdot \text{legal\_score}_i|}{|\beta_{\text{legal}} \cdot \text{legal\_score}_i| + |\beta_{\text{load}} \cdot \text{load\_at\_filing}_i|}$$

$$\text{absolute\_load\_share}_i = \frac{|\beta_{\text{load}} \cdot \text{load\_at\_filing}_i|}{|\beta_{\text{legal}} \cdot \text{legal\_score}_i| + |\beta_{\text{load}} \cdot \text{load\_at\_filing}_i|}$$

**Statistical Safeguard:** These shares partition the fitted linear predictor of the model. They are **not** causal attributions of delay.

---

## Temporal Validation and Leakage

Constructing a similarity graph over a pooled cross-section of cases introduces temporal leakage: earlier-filed cases can acquire neighbours from cases filed years into the future.

### Naive vs. Leakage-Aware Validation
- **Naive Graphing:** Constructing `build_case_graph()` across all cases allows future cases to influence training representations. In audits of full-sample graphs, substantial proportions of historical cases acquire future neighbours.
- **Leakage-Aware Architecture:**
  1. Cases are split chronologically by sorting on `(filing_date, case_id)`.
  2. The earliest fraction (e.g. 70%) forms the training cohort; the remainder forms the validation cohort.
  3. The section graph, diffusion operator, and IDF weights are computed **strictly from training cases**.
  4. Validation cases are projected into the training legal space via `project_validation_legal_score()`.
  5. Validation cases connect **only to training cases** as neighbours.
  6. **Validation cases never form neighbours with each other**, preventing validation-to-validation contamination.
  7. Out-of-sample Harrell's C-index is evaluated using training model coefficients and projected validation scores.

### Leakage Audit Functions
- `check_future_neighbours()`: Flags graph edges connecting past cases to future cases across a temporal cutoff.
- `check_court_overlap()`: Distinguishes whether validation assesses generalisation across time within known courts or across unseen courts.
- `check_near_duplicates()`: Flags same-court, same-filing, same-decision clusters to inspect recording redundancy.

---

## Synthetic Benchmark Results

Evaluated on the canonical 80-case synthetic validation dataset:

| Dimension | Cohort Specification | Value |
|:---|:---|:---|
| **Total Cohort** | Total Cases ($N$) | 80 |
| | Observed Events | 59 |
| | Right-Censored (Pending) | 21 |
| **Training Cohort (70%)** | Training Cases ($N_{\text{train}}$) | 56 |
| | Observed Events | 41 |
| | Right-Censored | 15 |
| **Validation Cohort (30%)** | Validation Cases ($N_{\text{val}}$) | 24 |
| | Observed Events | 18 |
| | Right-Censored | 6 |
| **Validation Discrimination** | **Held-Out Harrell's C-index** | **0.5038** |

### Honest Interpretation of Synthetic Discrimination:
The held-out concordance index of **0.5038** on this synthetic dataset is approximately chance-level discrimination ($0.50$). Because the synthetic outcomes were generated independently of the statutory profiles and docket load features, an out-of-sample C-index near 0.50 is the expected statistical baseline. This confirms correct execution of the leakage-free training-to-validation projection pipeline without numerical collapse or data fabrication; it does NOT demonstrate predictive success or empirical findings.

---

## Interpretation and Methodological Limitations

Researchers using `docketscape` must adhere to the following 12 explicit methodological constraints:

1. **Observational Nature:** All models and metrics describe observational associations in judicial records.
2. **Non-Causal Docket Load:** Congestion associations do not estimate the causal impact of court backlog on delay.
3. **Representation vs. Legal Merit:** Legal similarity captures shared statutory provisions, not the substantive merit, complexity, or strength of legal arguments.
4. **No Judicial Misconduct Claims:** Relative lateness measures position within an empirical neighbour survival distribution, not judicial wrongdoing or administrative negligence.
5. **No Judge Quality Scores:** Court load reflects court-level docket volume, not the diligence, effort, or competence of presiding judges.
6. **No Judge Ranking:** The package does not rank, score, or compare individual judges.
7. **No Efficiency Estimation:** The package does not compute judicial efficiency, productivity frontiers, or staffing benchmarks.
8. **No Causal Mechanisms:** Decomposing linear predictors does not establish causal mechanisms of delay.
9. **Data Quality Sensitivity:** Output validity depends entirely on the completeness and accuracy of underlying court records (e.g., disposal classification, section entry accuracy).
10. **Censoring Assumptions:** The validity of Kaplan-Meier and Cox estimators relies on non-informative right censoring. If cases remain pending due to unobserved systematic factors, survival curves may be biased.
11. **Synthetic Validation Status:** Synthetic pipeline demonstrations validate package implementation, not empirical dynamics in Indian courts.
12. **Discrimination Expectations:** A validation C-index near 0.5 indicates weak or absent discrimination in that specific validation setting and should be reported honestly without inflation.

---

## Development Data Lab (DDL) Attribution & Data Licensing

`docketscape` was conceptualized for analyzing case-level judicial data from the **Development Data Lab (DDL)** Indian lower-court dataset ([devdatalab.org](https://www.devdatalab.org)).

- **Conceptual & Motivating Role Only:** DDL represents the real-world dataset structure that motivated the package's design (case identifiers, filing dates, decision dates, statutory acts/sections).
- **No Empirical Data Bundled:** `docketscape` contains **zero empirical DDL records**. All datasets in this package, including package tests, examples, and the canonical 80-case validation benchmark, are strictly generated synthetic data structures.
- **No Empirical Claims:** The package does not claim, establish, or report any empirical findings regarding Indian lower courts.
- **Independent Data Sourcing & Terms:** Researchers wishing to analyze empirical DDL data must obtain it directly from Development Data Lab ([devdatalab.org](https://www.devdatalab.org)) and independently ensure compliance with DDL's specific terms of use, licensing, and citation requirements. `docketscape` neither redistributes DDL data nor grants or invents any licensing or redistribution permissions on behalf of Development Data Lab.

---

## Public API Reference

| Function | Primary Purpose |
|:---|:---|
| [`clean_cases()`](#) | Standardises dates, establishes right-censored durations, flags invalid records. |
| [`build_section_graph()`](#) | Constructs statutory section co-occurrence network from case citations. |
| [`diffuse_profiles()`](#) | Smooths case citation vectors over section network via one-step weighted profile diffusion. |
| [`build_case_graph()`](#) | Builds $k$-NN case similarity graph with IDF weighting and deterministic tie handling. |
| [`neighbourhood_survival()`](#) | Estimates weighted Kaplan-Meier expected timeline from legally similar neighbours. |
| [`relative_lateness()`](#) | Evaluates case duration percentile relative to neighbour survival experience. |
| [`court_load()`](#) | Computes monthly open court docket load. |
| [`fit_delay_model()`](#) | Fits Cox proportional hazards regression on legal score and filing-time court load. |
| [`decompose_delay()`](#) | Partitions linear predictor into legal and court load absolute contribution shares. |
| [`validate_models()`](#) | Evaluates out-of-sample temporal discrimination and leakage-aware projection. |
| [`project_validation_legal_score()`](#) | Projects validation cases into training legal representation without future leakage. |
| [`check_future_neighbours()`](#) | Audits graph edges connecting past cases to future cases. |
| [`check_court_overlap()`](#) | Audits court representation overlap across training and validation splits. |
| [`check_near_duplicates()`](#) | Audits same-date/court clusters and exact duplicate records. |

---

## Citation

If you use `docketscape` in your research, please cite:

```bibtex
@manual{docketscape2026,
  title  = {docketscape: Legal Network and Congestion-Aware Delay Modeling for Court Dockets},
  author = {Shraddha Ajit and DK Janani},
  year   = {2026},
  note   = {R package version 0.1.0},
  url    = {https://github.com/shraddhaajit/dockerscape}
}
```

---

## Authors

- **Shraddha Ajit** — <shra19076@gmail.com> (Author, Creator)
- **DK Janani** — <janani007dk@gmail.com> (Author)

---

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
