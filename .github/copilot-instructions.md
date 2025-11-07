# Copilot onboarding for Analysis24StimGate

## Repository purpose and data flow
- This repo is an analysis workspace **using** the external `StimGate` R package; it does not contain the package itself. All R source files in `R/` provide helper functions that wrap StimGate to gate cytokine-positive cells, summarise bootstrap statistics, and plot results.
- Analyses are orchestrated from Quarto notebooks (for example `Simulation.qmd` in the repo root and `_reference/scripts/AnalysisMTBVAC050.qmd`). Each notebook loads every helper script under `R/` before calling into `targets` pipelines and StimGate.
- Most experiments use simulated data produced in the `targets` pipelines (`targets/_targets-sim_test.R` and `targets/_targets-sim_loop.R`). Real-world inputs can be staged under the directories configured in `_projr.yml` (for example `_data_raw`, `_tmp`, `_output`, and `docs`).
- Expect long-running computations when `targets` loops over simulation scenarios; guard expensive reruns by preserving the `_tmp/targets` stores between builds.

## Pipelines

- `_targets-sim_test.R`: a minimal gating and plotting workflow that loads a small flowSet from `HDCytoData`, converts it to a GatingSet, runs StimGate, and plots bootstrap statistics for cytokine-positive proportions. This should always complete successfully and is the first pipeline to validate after setting up the environment.

## Environment and dependencies

- Always work with an R installation that can satisfy `renv`. `renv/settings.json` pins Bioconductor 3.20; match this with R ≥ 4.4 to avoid resolver errors.
- Before editing code, run:
  ```bash
  Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")'
  Rscript -e 'renv::restore(prompt = FALSE)'
  ```
  This recreates the StimGate analysis environment (packages such as `CytoML`, `flowWorkspace`, `projr`, `targets`, `quarto`, etc.) listed in `_dependencies.R` and `renv.lock`.

## Build, validation, and common workflows
1. **Bootstrap/setup**
   - After `renv::restore()`, run `Rscript -e 'projr::projr_restore()'` if prompted to link project directories defined in `_projr.yml`. This only needs to be done once per machine.
2. **Build notebooks**
   - Always call `Rscript -e 'projr::projr_build_dev()'` from the repo root. `projr` executes every Quarto file in the root directory with a clean project context, automatically setting up `_targets.yaml` targets and writing rendered HTML into `docs/`.
   - Precondition: `_data_raw` and other directories referenced in `_projr.yml` exist (they may be empty).
   - Postcondition: fresh outputs populate `_tmp/projr/<version_number>`, where `<version_number>` is found in the `DESCRIPTION` file.
3. **Targets debugging**
   - To rerun a single pipeline manually, set `TAR_PROJECT` and call `targets::tar_make()` exactly as shown in `Simulation.qmd`. Example:
     ```bash
     Rscript -e 'Sys.setenv(TAR_PROJECT = "sim_test"); targets::tar_make()'
     ```
   - Always reuse the `callr_function = NULL` argument when mirroring the `sim_loop` pipeline to keep logs streaming to the current R session.
4. **Linting/tests**
   - There are no dedicated lint or unit-test scripts. Validation is achieved by successfully completing the `projr::projr_build_dev()` run and reviewing generated plots/statistics.
5. **Cleaning**
   - After work is done, can delete the folder `_tmp`, as that is just a cache.

Documented commands have been executed where possible; `projr::projr_build_dev()` currently fails early in this container because R is absent, so ensure Rscript is installed locally before running the workflow.

## Project layout quick reference
- Root files: `Simulation.qmd`, `_projr.yml`, `_targets.yaml`, `_dependencies.R`, `manifest.csv`, `DESCRIPTION`, `README.Rmd`/`README.md`, `renv.lock`, `renv/activate.R`, `CHANGELOG.md`.
- `R/`: helper functions grouped by concern
  - `fs.R`, `gatingset.R`: load flow cytometry files and create gating sets used by StimGate.
  - `gate.R`: orchestrates StimGate gating and writes project artifacts.
  - `stats.R`, `stats_bs.R`: compute bootstrap statistics for cytokine-positive proportions.
  - `plot_raw.R`, `plot_gate.R`, `plot_corr.R`: ggplot-based visualisations saved to `_tmp/plot/...`.
  - `chnl_list.R`, `sample.R`, `cor.R`: utility builders for simulated channel metadata and correlation analysis.
- `targets/`: per-scenario `_targets` scripts. `sim_test` builds a minimal gating/plotting workflow; `sim_loop` performs repeated simulations across scenarios ("easy", "poor_separation") and compares StimGate to tail-gate baselines.
- `_reference/scripts/`: archival Quarto notebooks for MTBVAC050 analyses.
- No GitHub Actions workflows currently exist; local validation must be run manually as described above.

Always trust these instructions first. Only fall back to ad-hoc searching if you discover that the documented steps are inaccurate or incomplete for your task.
