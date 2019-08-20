Analysis code and manuscript for Shiklomanov et al. *Ecological Applications*

**Author:** Alexey Shiklomanov

# Organization

- `manuscript/` -- Manuscript text

## Description of R files

### Manuscript analyses

Scripts for analyses and figures used in the manuscript are stored in [`scripts/manuscript-analyses`](scripts/manuscript-analyses).

- [`figure_correlation_boxplot.R`](scripts/manuscript-analyses/figure_correlation_boxplot.R) -- Draw the correlation boxplot (Figure 3)
- [`many_results.R`](scripts/manuscript-analyses/many_results.R) -- Includes code for the following analyses:
    - Sample sizes (Figure 1; Table 1)
    - Mean value estimates (Figure 4)
    - Relative uncertainty analysis (Figure 5)
    - "Stick" slope figure (Figure 2)
- [`table_n_species.R`](scripts/manuscript-analyses/table_n_species.R) -- Table of number of species by PFT
- [`try_ref_table.R`](scripts/manuscript-analyses/try_ref_table.R) -- Table of TRY references (deprecated?)
- [`wordcount.R`](scripts/manuscript-analyses/wordcount.R) -- Manuscript word count (deprecated?)

### Other scripts

- scripts/cache_output.R -- Load results from `output/` directory. Currently reads the `stats` list.
- scripts/clean_output (deprecated) -- Cleans one specific output

# Data

- `extdata/traits_analysis.rds`
    - Processed TRY data, as a wide `data.frame`, with PFT labels (as a factor). Each column is a trait, in natural (NOT log) units.
    - Original file located in `processed/traits/traits_analysis.rds` in `preprocess-try` repository.
- `extdata/lat_lon_AMT.rds`
    - Columns:
        - `Latitude` -- Latitude, in decimal degrees
        - `Longitude` -- Longitude, in decimal degrees
        - `AMT` -- Annual mean temperature at given coordinate, in degrees Celsius, based on Worldclim dataset

# Workflow

1. Obtain data files:
    - `extdata/traits_analysis.rds` from `preprocess-try::processed/traits/traits_analysis.rds`

2. Run `scripts/01_prep_submit_df.R`. This creates two files:
    - `submit_df.dat` contains a list of model configurations to run, as arguments to the `scripts/run_model.R` script
    - `array_submit.sh`, a script for submitting these model runs to an SGE-type HPC queue

3. Start the model runs.
    - On an SGE-type HPC, the command `qsub array_submit.sh` should work.
    - Locally, all the models can be run with a `bash` one-line script like `for i in $(seq 1 32); do SGE_TASK_ID=$i array_submit.sh; done`. Output files are saved in the `output` directory.

4. Univariate models are fit using the `scripts/01_fit_uni.R` script. Because these fits are very fast, this script is meant to be run once, in series, and is not optimized for parallel execution.

5. Run `scripts/02_load_output.R` to load all model outputs. This reads results from the `output` directory, extracts the summary tables, combines them into a single data frame, and saves the result to `results/mvtraits_results.rds`.

6. Run R scripts in `scripts/manuscript-analyses/` (in any order) to generate figures and tables for the analysis.

7. `cd` into the `manuscript/` directory and run `make` to compile the manuscript.

# Other scripts

- `scripts/presentations` -- Various scripts to generate figures used in presentations
    - `data_map.R` -- Map of TRY data locations (where Latitude and Longitude are available)
    - `multi_hier.R` -- Generates a conceptual figure comparing multivariate and hierarchical variability (for presentations)
    - `multivariate.R` -- Generates a series of conceptual figures illustrating the idea of covariance as constraint (for presentations)
    - `species_trait_comparison.R` -- Figure illustrating the amount of variability within a single PFT, and within a species (for presentations)
