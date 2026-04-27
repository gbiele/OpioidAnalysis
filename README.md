# Replication Package: Effects of Psychosocial Stress on Opioid Self-Administration in Healthy Participants

This repository contains the data and analysis code for the paper:

> Eikemo, M., Løseth, G. E., Carlyle, M., Trøstheim, M., Ernst, G., Pazmandi, C., Thompson, M., Vezzani, C., Meier, I. M., Roland, M. N., Johnstone, T., Heilig, M., Biele, G., & Leknes, S.
> *Statistical analyses and plots for <br>"Effects of Psychosocial Stress on Opioid Self-Administration in Healthy Participants: A Randomized, Placebo-Controlled Crossover Trial.*

- **Preregistration:** <https://osf.io/bcxv8>
- **Preprint:** <https://osf.io/preprints/osf/v8dxy>

Statistical analyses were performed by Guido Biele, in collaboration with the author team.

## Repository Structure

```
.
├── OPIOIDREWARD_WP1.Rmd           Full replication report (Bookdown HTML); knit from project root
├── OPIOIDREWARD_WP1_excerpt.Rmd   Shorter report: core ordinal analyses only; also knit from project root
├── buffer.Rmd                 Child fragment (included from main report; do not knit alone)
├── drug_side.Rmd              Child fragment (do not knit alone)
├── drug56.Rmd                 Child fragment (do not knit alone)
├── drug_want_like.Rmd         Child fragment (do not knit alone)
├── R/
│   ├── setup.R                Package loading, ggplot themes, color scales
│   └── utils.R                Custom functions (model fitting, plotting, reporting)
├── data/
│   ├── my_data_Questionnaire.Rdata   VAS questionnaire ratings
│   ├── my_data_cortisol.Rdata        Plasma cortisol measures
│   ├── my_data_SelfAdmin.Rdata       Self-administration task data
│   ├── dosage_data.Rdata             Drug dosage information
│   ├── HR.Rdata                      Heart rate data
│   ├── HRHRVdata.csv                 Heart rate / HRV summary data
│   ├── HRVactivity.Rdata             Activity-level HR (stress time-course figure)
│   └── Times.Rdata                   Experimental timing data
├── assets/                    Image assets used in time-course plots
├── references.bib             Bibliography
└── OPIOIDREWARD_WP1.html     Pre-rendered report
```

**Data metadata:** Variable definitions and related documentation are in [`data/DATA_DICTIONARY.md`](data/DATA_DICTIONARY.md).

## Quick Start

If you only want to inspect the results, open **`OPIOIDREWARD_WP1.html`** in a web browser. It contains the full rendered report with all figures, tables, and statistical output. After knitting **`OPIOIDREWARD_WP1_excerpt.Rmd`**, open **`OPIOIDREWARD_WP1_excerpt.html`** for the shorter core-analysis report.

To reproduce the analyses from source, follow the instructions below.

## Which files to knit (standalone reports)

**Two `.Rmd` files are meant to be knitted on their own** from the project root (the folder that contains `data/` and the main report):

- **`OPIOIDREWARD_WP1.Rmd`** — full replication report (all outcomes, figures, and supplementary material in that document).
- **`OPIOIDREWARD_WP1_excerpt.Rmd`** — trimmed excerpt with shared data preparation and the **core ordinal regression analyses** only (effect obtained, including the prior sensitivity and sex-stratified variants, and stated desired effect). It omits much of the full manuscript (other outcomes, time courses, large QC tables, extra sensitivities, etc.) and is useful when you want a **faster, smaller HTML** run without the rest of the pipeline.

Use the same working directory, prerequisites, and package setup for both.

The auxiliary files (`buffer.Rmd`, `drug_side.Rmd`, `drug56.Rmd`, `drug_want_like.Rmd`) are **not** standalone analyses. They are woven into the main report with `knitr::knit_child()` or `child=` chunk options and assume that the main document has already:

- sourced `R/setup.R` and `R/utils.R`,
- loaded all prepared data from `data/`, and
- created objects in the R session such as `my_data`, `my_data.Q`, and other derived tables.

If you open one of those child files and click **Knit**, or run their chunks in a fresh session, you will typically see errors such as **`object 'my_data.Q' not found`** (or similar), even when every data file on disk is present. That is expected: those objects are produced by earlier code in **`OPIOIDREWARD_WP1.Rmd`**, not by the child file itself.

**Correct workflow:** set the working directory to the project root, then knit **`OPIOIDREWARD_WP1.Rmd`** for the full report, or **`OPIOIDREWARD_WP1_excerpt.Rmd`** for the excerpt only.

## Prerequisites

### R and Stan

- **R** (developed with R 4.x)
- **CmdStan** and the **cmdstanr** R package. Many of the Bayesian models use `backend = "cmdstanr"`. Follow the [CmdStanR installation guide](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) to set up CmdStan.

### Hardware

- **RAM:** **32 GB or more is recommended** for Stan / CmdStan with these models. Runs are possible on machines with less memory, but you may hit swapping or out-of-memory errors depending on model size and parallel sampling.
- **CPU:** Wall-clock time depends strongly on core count and speed; the analysis uses `options(mc.cores = 4)` by default (see `R/setup.R`).

### R Packages

Install all required packages before knitting. The following packages are loaded by the analysis:

```r
install.packages(c(
  # Reporting and tables
  "knitr", "kableExtra", "flextable", "bookdown",

  # Data manipulation
  "magrittr", "data.table", "collapse",

  # Bayesian modelling
  "brms", "posterior", "bayesplot", "rstanarm", "cmdstanr",

  # Plotting
  "ggplot2", "ggdist", "GGally", "lemon", "sjPlot",
  "patchwork", "ggbeeswarm", "ggh4x", "ggthemes",
  "colorspace", "viridis", "ggimage",

  # Misc
  "gtsummary", "hausekeep", "grateful", "stringr"
))
```

> **Note:** `cmdstanr` is not on CRAN. Install it with:
> ```r
> install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
> ```
>
> **Important — CmdStan and C++ toolchain:** Installing the `cmdstanr` R package is only the first step. Before you can run any models, you must also install CmdStan itself (the latest version) by calling `cmdstanr::install_cmdstan()`. CmdStan compiles Stan programs to native binaries, so **a C++ compilation toolchain must already be present on your machine** (e.g. Rtools on Windows, Xcode Command Line Tools on macOS, or `build-essential` on Linux). The required toolchain differs by operating system and R version. We strongly recommend reading the [CmdStanR installation guide](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) and the [CmdStan system requirements](https://mc-stan.org/docs/cmdstan-guide/installation.html) carefully before proceeding. If you run into toolchain or compilation issues, the [Stan Forums](https://discourse.mc-stan.org/) are the best place to seek help.

> **Note:** `hausekeep` is not available for all versions of R. Install it with:
> ```r
> install.packages("remotes")
> remotes::install_github("hauselin/hausekeep")
> ```


## Reproducing the Analysis

Reproduce from a **standalone** report (see [Which files to knit (standalone reports)](#which-files-to-knit-standalone-reports) above). Open the chosen `.Rmd` in RStudio and knit to HTML, or run from the R console:

**Full report**

```r
bookdown::render_book("OPIOIDREWARD_WP1.Rmd", output_format = "bookdown::html_document2")
```

**Excerpt (core analyses only)**

```r
bookdown::render_book("OPIOIDREWARD_WP1_excerpt.Rmd", output_format = "bookdown::html_document2")
```

### What happens during knitting

1. `R/setup.R` and `R/utils.R` are sourced (packages, themes, helper functions).
2. Output directories (`brmsP1/`, `fits/` and subdirectories) are created automatically if they do not exist.
3. Prepared data files are loaded from `data/` (the full report also uses `HRVactivity.Rdata` for the heart-rate panel in the stress time-course figure).
4. For each Bayesian model the code checks whether a saved fit already exists (e.g. `brmsP1/stressed_acat.Rdata`). If yes, it loads the cached result; if not, it fits the model from scratch via **brms/CmdStan**. Stan models are sampled using **4 MCMC chains**, run in parallel.
5. In the **full** report only, `grateful-refs.bib` (the second bibliography file listed in the YAML header) is generated automatically by the **grateful** package during knitting.

### Runtime

A **full refit** of the **main** report (every Bayesian model estimated from scratch, with no saved fits under `brmsP1/` or `fits/`) typically takes **longer than one standard workday** (i.e. more than roughly 9am–5pm on a single calendar day). Exact wall time varies a lot by machine (CPU, RAM, parallel chains, and whether models compile or load from cache). The **excerpt** fits fewer models and skips large parts of the manuscript, so wall time is usually **substantially shorter** than a full main-report refit, subject to the same caching behavior.

If you already have fitted objects from a previous run, subsequent knits are much faster because the code loads cached `.Rdata` files instead of re-estimating.

## Sensitive Data (Not Included)

Two data files contain sensitive participant information and are **not shared** in this repository:

- `data/NicotineUse.csv`
- `data/ContraceptiveUse.csv`

The code handles their absence gracefully via `file.exists()` checks. When these files are missing, the corresponding sensitivity analyses (contraceptive and nicotine adjustment models) are skipped, and a note is displayed in the rendered document. These files may be available from the authors upon reasonable request.

## License and Citation

Please cite the associated paper and preregistration when using this replication package. See the preprint linked above for the full citation.
