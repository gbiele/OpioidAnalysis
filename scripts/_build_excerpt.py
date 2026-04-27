from pathlib import Path

src = Path("OPIOIDREWARD_WP1.Rmd")
lines = src.read_text(encoding="utf-8").splitlines(keepends=True)


def sl(a, b):
    return "".join(lines[a - 1 : b])


out = []
yaml = sl(1, 17)
yaml = yaml.replace(
    "title: 'Statistical analyses and plots for \\n\"Stress-enhanced opioid self-administration in healthy men\"'",
    "title: 'Excerpt: OLR obtained and stated desired effect (from full report)'",
)
yaml = yaml.replace(
    "bibliography: [references.bib, grateful-refs.bib]",
    "bibliography: references.bib",
)
out.append(yaml)

intro = """This document is a **trimmed excerpt** of the full replication report. It keeps only the data preparation needed for three analyses—adjacent-category OLR for *effect obtained* (with prior sensitivity), sex-stratified OLR for *effect obtained*, and stated *desired* effect—and drops the rest of the manuscript (other outcomes, time courses, extensive QC tables, sensitivity adjustments, etc.).

For preregistration context, full descriptives, other models, and appendices, see the complete source: [OPIOIDREWARD_WP1.Rmd](OPIOIDREWARD_WP1.Rmd).

---

"""
out.append(intro)
out.append(sl(19, 42))

setup = sl(67, 84)
setup = setup.replace('source("setup.R")', 'source("R/setup.R")')
setup = setup.replace('source("utils.R")', 'source("R/utils.R")')
setup = setup.replace("LONG_MODE = TRUE", "LONG_MODE = FALSE")
out.append(setup)
out.append(
    '\n```{r ensure_dirs, include=FALSE}\n'
    'dir.create("brmsP1/obtained", recursive = TRUE, showWarnings = FALSE)\n'
    'dir.create("figures", recursive = TRUE, showWarnings = FALSE)\n'
    "```\n"
)

out.append("\n# Opioid self-administration: statistical excerpt\n\n")
out.append("## Data preparation\n\n")
out.append(
    "The chunks below match the full report's pipeline up to coarsening of *effect obtained* "
    "(oxycodone sessions only). For narrative around exclusions and compliance, see "
    "[OPIOIDREWARD_WP1.Rmd](OPIOIDREWARD_WP1.Rmd).\n\n"
)
out.append("### Load and prepare raw tables\n\n")
out.append(sl(90, 117))
out.append("### Self-administration summaries used downstream\n\n")
out.append(sl(130, 138))
out.append(sl(144, 147))
out.append(sl(150, 155))
excl = sl(157, 195)
excl = excl.replace("my_data.posng_change$filter", "my_data.posng_change$stress.filter")
out.append(excl)
out.append(sl(197, 210))
out.append(sl(221, 224))
out.append(sl(226, 247))
out.append(sl(249, 273))

out.append("\n### Effect obtained (oxycodone path)\n\n")
out.append(
    "Session-level self-admin data and dosage merge (same code as the full report).\n\n"
)
out.append(sl(2312, 2359))
out.append(sl(2470, 2492))

out.append(
    "\n```{r prep_session_factor, include=FALSE}\n"
    "# Cumulative OLR chunk omitted in this excerpt; session must be factor for brms.\n"
    "my_data_beh[, session := factor(session)]\n"
    "```\n"
)

out.append(
    "\n---\n\n## Ordinal and self-report analyses\n\n"
    "The following sections are copied from the full report. For the cumulative "
    "(proportional-odds) model and diagnostic figures that precede adjacent-category OLR "
    "in the full document, see [OPIOIDREWARD_WP1.Rmd](OPIOIDREWARD_WP1.Rmd).\n\n"
)

out.append(sl(2558, 2720))
out.append(
    "\nFor contraception/nicotine sensitivity and other extensions, see "
    "[OPIOIDREWARD_WP1.Rmd](OPIOIDREWARD_WP1.Rmd).\n\n"
)
out.append(sl(2721, 2832))
out.append(
    "\n---\n\n## Self reported effects (stated desired only)\n\n"
    "For DEQ take-drug-again and other items in the full chapter, see "
    "[OPIOIDREWARD_WP1.Rmd](OPIOIDREWARD_WP1.Rmd).\n\n"
)
out.append(sl(3365, 3470))

Path("OPIOIDREWARD_WP1_excerpt.Rmd").write_text("".join(out), encoding="utf-8")
print("Wrote OPIOIDREWARD_WP1_excerpt.Rmd")
