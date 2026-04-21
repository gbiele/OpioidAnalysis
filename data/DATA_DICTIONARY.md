# Data dictionary (README-listed files)

This document lists **variables present on disk** that appear in analysis code (`*.Rmd` in the project root and `R/utils.R`). Types and **observed non-numeric values** come from opening the data files in this workspace. `Sex`, `state.condition`, `induced.state`, `concentr`, and similar fields created only in the R session are omitted here per the replication plan.

**Enumeration:** `my_data_Questionnaire.Rdata`, `my_data_cortisol.Rdata`, `my_data_SelfAdmin.Rdata`, `dosage_data.Rdata`, `HR.Rdata`, `HRHRVdata.csv`, `HRVactivity.Rdata`, `Times.Rdata`, `ContraceptiveUse.csv`, and `NicotineUse.csv` are named in [README.md](../README.md). **`data/HRHRVdata.csv` is not present in this checkout** and is not referenced anywhere in `*.Rmd` or `R/*.R` in this repository.

---

## my_data_Questionnaire.Rdata

**Shape:** Long-format trial-level table (`my_data`). One row is one VAS (or checklist) response for a given **participant**, **session**, experimental **stage**, and **item_name** (multiple items per stage).

| variable name | variable type | description |
|---------------|---------------|-------------|
| participant | integer (observed 1–123) | Indexing: numeric participant ID; joins to other tables and used everywhere for grouping, filtering, and random effects. |
| session | integer (observed 1–4) | Indexing: within-subject session index (crossover design); used in summaries, merges, and as a random effect. |
| stage | numeric (observed 1–11) | Indexing: trial/stage index within a session (maps to protocol phases such as baseline, induction, reminders); used in `make_the_data`, plots, and models as `stage` / factor `Stage`. |
| item_name | character (38 observed item slugs; includes e.g. anxious, distressed, stressed, drug_like, …) | Indexing: which questionnaire item the row belongs to; subset via `select_data()` and item lists (`negaffect_items`, etc.); `dull` renamed to `blunted` in code. |
| response | numeric (observed 0–100) | Value: VAS or analogue rating for that item at that stage; main outcome in ordinal models after coarsening. |
| Stress | factor (levels: `control`, `stress`) | Value / design factor: assigned stress vs control induction; drives `state.condition` and `induced.state` in code and enters formulas. |
| Drug | character (`oxycodone`, `placebo`) | Value / design factor: drug arm; used in stratification, merges, and models (one session manually recoded in analysis). |
| gender | character (`f`, `m`) | Value: recorded sex at recruitment; used to derive `Sex` (`men` / `women`) for all downstream analyses. |

---

## my_data_cortisol.Rdata

**Shape:** Long-format biosample-level table (`my_data`). One row is one cortisol assay / sample metadata row linked to **participant**, **session**, **stage**, and design factors; multiple technical repeats can share a **Sample.ID** (aggregated in code with `mean(result)`).

| variable name | variable type | description |
|---------------|---------------|-------------|
| participant | integer | Indexing: participant ID; filtered to self-admin cohort and used in cortisol models and plots. |
| session | integer | Indexing: session index; passed to models (often as factor) and summaries. |
| stage | integer (observed 2–10 in file) | Indexing: protocol stage associated with the sample; used in spline models and contrasts (e.g. pre/post induction). |
| Sample.ID | integer | Indexing: lab sample identifier; used to aggregate replicate `result` values into `concentr` before analysis. |
| plate | numeric | Indexing / batch covariate: microplate number for the assay run; kept in the reduced cortisol table used for modelling. |
| result | numeric | Value: raw assay read (scaled in Rmd by `*100*0.0001` then logged); averaged within `Sample.ID` to form concentration. |
| gender | character (`f`, `m`) | Value: used in cortisol subset alongside drug/stress/time covariates (same coding as questionnaire). |
| time | numeric | Value: sampling time (fraction of day) used as a smooth term in Bayesian cortisol models. |
| Drug | character (`oxycodone`, `placebo`) | Design factor for drug arm (session fix applied in code). |
| Stress | factor (`control`, `stress`) | Design factor for stress arm. |

---

## my_data_SelfAdmin.Rdata

**Shape:** Long-format within-session time series (`my_data`). One row is one timestep of the self-administration cursor task for a **participant** × **session**; many rows per session with **time** advancing.

| variable name | variable type | description |
|---------------|---------------|-------------|
| participant | integer | Indexing: participant ID for grouping, compliance checks, and merges. |
| session | integer | Indexing: session index; used with participant in keys and summaries. |
| time | numeric (seconds within task) | Indexing / covariate: task clock; used to find first time to target, last trial, and time-series plots. |
| target | numeric (0–125) | Value: instructed target on the 0–125% effect scale; used with `effect_obtained` for compliance (`cheater`), dosing theory, and reporting. |
| effect_obtained | numeric | Value: current self-reported “obtained” effect (% scale); main behavioural outcome, coarsened for ordinal models. |
| position | numeric | Value: cursor / joystick position on the response axis; exported in tables comparing final position to target. |
| p_down | integer | Value: count of downward button presses in the window; summed with `p_up` for “number of presses” analyses. |
| p_up | integer | Value: count of upward button presses; combined with `p_down`. |
| bias | numeric | Value: task bias parameter passed into `get_ftt()` to determine direction of “hit target” logic. |
| satisfact | numeric (many NA) | Value: end-of-task satisfaction rating on the same scale; used to filter last-trial rows when NA. |
| Drug | character (`oxycodone`, `placebo`) | Design factor. |
| Stress | factor (`control`, `stress`) | Design factor. |
| gender | character (`f`, `m`) | Used with self-admin data when deriving `Sex` alongside questionnaire rows. |

---

## dosage_data.Rdata

**Shape:** One row per **participant** × **session** (`dosage_data`); wide clinical/admin fields for that visit.

| variable name | variable type | description |
|---------------|---------------|-------------|
| participant | integer | Indexing: merges to behavioural and questionnaire tables. |
| session | character (`1`, `2`, `3`, `4`) | Indexing: session label (coerced alongside other tables in reporting chunks). |
| weight_kg | character in file → numeric in R | Value: body weight (kg); parsed with comma decimals, imputed by participant mean when missing, and used to compute mg/kg doses. |
| drug_1_dose | character in file → numeric in R | Value: first oxycodone/placebo dose (mg); used in dose summaries and `drug_2_dose_theory`. |
| drug_2_dose | character in file → numeric in R | Value: delivered second dose (mg); used in summaries, omission flags, and mg/kg reporting. |

---

## HR.Rdata

**Shape:** Long-format heart-rate series (`HR`). One row is **Participant** × **Session** × **time** (seconds from state-induction midpoint) with averaged beats-per-second.

| variable name | variable type | description |
|---------------|---------------|-------------|
| Participant | character (`E001`–style IDs) | Indexing: coerced to numeric `participant` after stripping `E`; used in aggregation and random effects. |
| Session | integer | Indexing: session number paired with participant. |
| time | numeric (−1200 to 1200) | Indexing / covariate: seconds relative to induction midpoint; basis of time-course plots and splines. |
| BPS | numeric | Value: heart rate as beats per second (converted to BPM in summaries); outcome in HR trajectory model. |
| Gender | character (`Men`, `Women`) | Design factor mapped to `Sex` for stratification and interactions. |
| Drug | character (`Oxycodone`, `Placebo`) | Design factor (title case in file; harmonised in code). |
| Condition | character (`Color`, `Music`, `Singing`, `TSST`) | Value: pleasant-activity vs stressor protocol variant for that session. |
| Stress | character (`Non-stress`, `Stress`) | Value: stress vs non-stress label aligned with `state.condition` in code. |

---

## HRVactivity.Rdata

**Shape:** Long-format segment summaries (`HR.activity`). One row is **Participant** × **Session** × experimental **Activity** window with median HR (`BPS`) and segment timing fields.

| variable name | variable type | description |
|---------------|---------------|-------------|
| Participant | character | Indexing: converted to numeric participant ID; filtered to analysis cohort. |
| Session | integer | Indexing: session index paired with participant. |
| Activity | character (31 observed codes, e.g. `script`, `Q1`–`Q11`, `self_admin`, `bandit`, …) | Indexing: which scheduled segment the HR segment belongs to; used for factor ordering, filtering, and boxplot time-course models. |
| a.start | integer | Value: segment start timestamp (arbitrary clock units); aligned to `script` start to build `a.start.0`. |
| duration | integer | Value: segment length in same units as `a.start`; used to compute midpoints and filter plausible segments. |
| BPS | numeric | Value: heart rate (beats per second) within the segment; scaled for the Gaussian `brm` model of activity effects. |
| Drug | character (`Oxycodone`, `Placebo`) | Carried through as a stratification covariate in the activity-level dataframe. |
| Condition | character (`Color`, `Music`, `Singing`, `TSST`) | Pleasant vs stressor manipulation label for the visit. |
| Stress | character (`Non-stress`, `Stress`) | Used once to derive `state.condition` before columns are dropped in `plot_tc_HeartRate`. |

---

## Times.Rdata

**Shape:** Schedule metadata (`TS`). One row is one named **Activity** with protocol timing relative to the reference timeline.

| variable name | variable type | description |
|---------------|---------------|-------------|
| Activity | character (49 codes, e.g. `drug_1_admin`, `Q3`, `state_prepmainmath`, …) | Indexing: merges questionnaire stages to clock times for timeline figures. |
| Activity_start | numeric | Value: activity start on the unified timeline (minutes / hours scale used in `utils.R`); merged to VAS curves. |
| Activity_end | numeric | Value: activity end time on the same scale; used when building intervals for plots. |
| Activity_mid | numeric | Value: midpoint used as the x-position for stage-average plots and alignment with HR overlays. |

---

## ContraceptiveUse.csv

**Shape:** One row per **participant** (character IDs in file; converted to integer in analysis).

| variable name | variable type | description |
|---------------|---------------|-------------|
| participant | character (`E001`, …) | Parsed to integer participant ID (`gsub("E", …)`); merge key into sensitivity ordinal models. |
| sex | character (`f`, `m`, `x`) | Used to zero out `contraceptives` for male participants before modelling. |
| contraceptives | character in file (`0`, `1`, `na`, `post menopause`) → numeric in R | Covariate: hormonal contraception / reproductive status coding; enters `brms` formula as numeric adjustment in contraception sensitivity analysis. |

---

## NicotineUse.csv

**Shape:** One row per participant (`ID`).

| variable name | variable type | description |
|---------------|---------------|-------------|
| ID | integer | Renamed to `participant`; merge key for nicotine-adjusted models. |
| nicotine | integer (column renamed from `Nicotine ordinal`; observed 0–2 with some NA) | Covariate: ordinal tobacco/nicotine burden code after column rename; only this column (besides `participant`) is merged into `my_data_beh`. |

*(Columns `Tobacco` and `daily nicotine` exist in the CSV with observed levels such as `Never`, `Occasionally`, `Daily >5` and binary 0/1, but they are not referenced in the scanned analysis code after ingest.)*

---

### Generation note

Column inventories and unique non-numeric values were taken by loading each file in R (`load` / `data.table::fread`) on 2026-04-20. Re-run the same checks if the underlying `.Rdata` / `.csv` files change.
