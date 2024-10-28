# README


The purpose of this compendium is to
`[briefly describe the compendium's goals and objectives]`

## Contact

For more information, please contact:
- `[Name]`, `[Email Address]`
- `[Name]`, `[Email Address]`

## Links

- `[URLs to data sources (e.g. OneDrive), GitHub repos, publications, etc.]`

## Details

### Reproduction instructions

#### GitHub Codespaces

- Ensure that the GitHub Codespace has access to the following environment variables:
  - `GH_TOKEN`: Classic PAT for GitHub
- Open GitHub Codespace
- Open terminal in repo you wish to run:
  - Ctrl + Shift + ` (Ctrl + Shift + backtick; backtick often found top-left of keyboard)
  - Choose repo from drop-down menu.
- Start `R`: run the command `radian`. Within `R`:
  - *Restore R packages*: Run `renv::restore(prompt = FALSE)`
  - *Build project*: Run `projr::projr_build_dev()`
