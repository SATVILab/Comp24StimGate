# Copilot onboarding for Analysis24StimGate

## Repository purpose and data flow
- This repo is an analysis workspace **using** the external `StimGate` R package; it does not contain the package itself. All R source files in `R/` provide helper functions that wrap StimGate to gate cytokine-positive cells, summarise bootstrap statistics, and plot results.
- Analyses are orchestrated from Quarto notebooks (for example `Simulation.qmd` in the repo root and `_reference/scripts/AnalysisMTBVAC050.qmd`). Each notebook loads every helper script under `R/` before calling into `targets` pipelines and StimGate.
- Most experiments use simulated data produced in the `targets` pipelines (`targets/_targets-sim_test.R` and `targets/_targets-sim_loop.R`). Real-world inputs can be staged under the directories configured in `_projr.yml` (for example `_data_raw`, `_tmp`, `_output`, and `docs`).
- Expect long-running computations when `targets` loops over simulation scenarios; guard expensive reruns by preserving the `_tmp/targets` stores between builds.

## Pipelines

- `_targets-sim_test.R`: a minimal gating and plotting workflow that loads a small flowSet from `HDCytoData`, converts it to a GatingSet, runs StimGate, and plots bootstrap statistics for cytokine-positive proportions. This should always complete successfully and is the first pipeline to validate after setting up the environment.

### sim_test Pipeline Details

The `sim_test` pipeline demonstrates StimGate's automated gating on simulated flow cytometry data. It consists of the following stages:

#### 1. Data Loading and Simulation
- **get_fs()** (R/fs.R): Downloads the Bodenmiller BCR-XL flowSet from HDCytoData as a template
- **get_chnl_list(fs)** (R/chnl_list.R): Generates simulated cytokine expression for two markers (BC1 and BC2):
  - BC1: 5% mean positive proportion with strong separation (expr_mean_pos = 2)
  - BC2: 1% mean positive proportion with strong separation (expr_mean_pos = 2)
  - Creates 8 batches, each with 2 samples (1 stimulated + 1 unstimulated)
  - Calls **sample_chnls()** (R/sample.R) which sequentially simulates each channel
  - **sample_chnl()** (R/sample.R) generates bimodal expression (positive/negative populations) using beta-distributed proportions

#### 2. GatingSet Creation
- **get_gatingset(fs, dir_cache)** (R/gatingset.R): Converts the simulated flowSet to a GatingSet and saves to cache directory

#### 3. Visualization of Raw Data
- **plot_raw(batch_list, chnl_list, fs, path_dir_save)** (R/plot_raw.R): Creates density plots showing expression distributions for stimulated vs unstimulated samples, faceted by batch

#### 4. Automated Gating
- **gate(chnl_list, batch_list, path_gs, path_project)** (R/gate.R): Runs StimGate's automated gating algorithm, which:
  - Accounts for batch effects
  - Subtracts unstimulated background
  - Determines optimal gates for each marker
  - Saves gates and statistics to path_project

#### 5. Statistics Computation
- **get_stats_tbl_bs_stimgate(path_project, chnl)** (R/stats_bs.R): Extracts StimGate-estimated proportions for:
  - Single markers (e.g., BC1+)
  - Combinations (e.g., BC1+BC2+, BC1+BC2-)
  - Any-positive (at least one marker positive)
- **get_stats_bs_actual(batch_list, chnl_list)** (R/stats_bs.R): Computes ground truth proportions from the known positive cell indices in the simulation
- Both functions use helper utilities:
  - **get_stats_bs_single()**: Marginalizes combination statistics to single markers
  - **get_stats_bs_any()**: Computes any-positive statistics

#### 6. Validation Plots
- **plot_corr(path_dir_save_base, stats_tbl_bs)** (R/plot_corr.R): Creates correlation plots comparing StimGate estimates vs actual proportions, showing concordance, Pearson, and Spearman correlations
- **plot_gate(batch_list, path_gs, path_project, marker, path_dir_save_base)** (R/plot_gate.R): Generates bivariate scatter plots with StimGate gates overlaid for each marker pair and batch

#### Key Data Structures
- **chnl_list**: A named list (one element per channel) where each element contains:
  - `fs`: flowSet with simulated expression values
  - `ind_list`: indices of positive cells in each sample
  - `resp_tbl`: metadata (batch, n_cell, prop_pos per sample)
  - `batch_list`: grouping of samples into batches
- **batch_list**: List of integer vectors, each defining sample indices in a batch (last index is always unstimulated)
- **stats_tbl_bs**: Tibble joining StimGate and actual statistics with columns:
  - `type`: "single", "combn", or "any"
  - `cyt`: marker combination string (e.g., "BC1(La139)Dd~+~BC2(Pr141)Dd~-~")
  - `sample_ind`: sample index
  - `prop_bs_stimgate`: StimGate-estimated background-subtracted proportion
  - `prop_bs_actual`: actual (ground truth) background-subtracted proportion

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
