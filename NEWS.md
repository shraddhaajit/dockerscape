# docketscape 0.1.0

## Initial Release

`docketscape` provides a statistical modeling framework for decomposing court delay into substantive legal complexity ("what a case is") and institutional congestion ("where the case sits") using network representations and survival analysis.

### Core Features

- **Case Standardization & Censoring (`clean_cases`)**:
  - Validates case-level filing and decision dates.
  - Handles right-censored pending matters relative to observation cutoffs.
  - Explicitly flags invalid records (e.g. decisions preceding filing, filings after observation end) without silent deletion.
  - Masks durations to `NA` for invalid cases to prevent non-positive durations in downstream models.

- **Statutory Network Representation (`build_section_graph`)**:
  - Constructs undirected section co-occurrence networks from case citations.
  - Employs sparse matrix crossproducts (`Matrix::crossprod`) for scalable graph construction.
  - Preserves edge weight invariants (strictly positive integer case co-occurrence counts).

- **Profile Diffusion (`diffuse_profiles`)**:
  - Smooths sparse statutory citation profiles across co-cited sections via one-step convex combination diffusion.
  - Enforces row-stochasticity (unit row sums, $L_1 = 1.0$) across all tuning values $\alpha \in [0, 1]$.

- **Case Similarity Graph (`build_case_graph`)**:
  - Constructs $k$-nearest-neighbour case graphs using inverse document frequency (IDF) weighted cosine similarities.
  - Uses chunked sparse evaluation and integer-ranked tie-breaking, scaling to large dockets within minimal memory.
  - Enforces graph invariants: no self-loops, no multigraph edges, maximum similarity weights on mutual edges.

- **Neighbourhood Survival & Relative Lateness (`neighbourhood_survival`, `relative_lateness`)**:
  - Computes weighted Kaplan-Meier survival curves from legally similar peers.
  - Evaluates relative lateness percentiles ($1 - S(T)$) anchored in empirical peer timelines.
  - Guards against extrapolation: flags unestimable cases when durations exceed peer follow-up.

- **Institutional Congestion Indexing (`court_load`)**:
  - Measures baseline court docket load at the monthly filing interval.
  - Implements fast $O(N \log N)$ `findInterval()` event tracking.

- **Semi-Parametric Delay Modeling (`fit_delay_model`, `decompose_delay`)**:
  - Fits Cox proportional hazards regression on legal scores and baseline court load.
  - Handles small sample sizes ($N < 3$ or zero events) gracefully without numerical crashes.
  - Decomposes linear predictors into legal and court load absolute contribution shares.

- **Leakage-Aware Validation & Auditing (`validate_models`, `project_validation_legal_score`, `check_*`)**:
  - Enforces strict chronological temporal holdouts preventing future-neighbour leakage.
  - Projects validation cases into training representations without validation-to-validation contamination.
  - Provides diagnostic audits: `check_future_neighbours()`, `check_court_overlap()`, and `check_near_duplicates()`.

### Methodological Safeguards

- Observational and non-causal framework.
- Does not rank judges, evaluate judicial performance, or assess judicial diligence.
- Contains zero bundled empirical DDL records; all package examples and tests use synthetic data structures.
