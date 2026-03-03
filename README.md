# Isotonic Phase I Design with Decreasingly Informative Priors

## Overview

This repository contains all code used to generate the results for our study on isotonic Phase I dose-finding designs incorporating decreasingly informative priors (DIP).

The repository includes:

- The isotonic method proposed by Conaway, Dunbar, and Peddada (CDP)
- The single-agent CDP implementation based on Conaway and Wages (2018)
- CDP with flat and non-flat priors
- The proposed Decreasingly Informative Prior (DIP) approach
- All simulation scenarios used in the manuscript
- Code to reproduce operating characteristics, tables, and results

The repository enables direct comparison between standard CDP-based approaches and the proposed DIP framework.

---

## Methods Implemented

### CDP (Conaway–Dunbar–Peddada)

- Single-agent isotonic design based on implementation by  Conaway & Wages (2018)
- Flat prior specification
- Non-flat prior specification

### Decreasingly Informative Prior (DIP)

The DIP approach reduces prior influence as sample size increases to mitigate early trial rigidity.

For scripts labeled with `DIP`:

- Posterior toxicity estimates may be computed using:
  - Posterior mean
  - Posterior mode
- The choice must be specified inside the function before running simulations.

---

## Simulation Scenarios

All simulation scenarios used in the manuscript are provided.

Outputs include:

- Dose selection percentages
- Patient allocation distributions
- Safety metrics
- Summary operating characteristics

---

## How to Run a Simulation

Example:

```r
source("R/utility_functions.R")
source("R/dip_mean.R")
source("simulations/scenario_definitions.R")

results <- run_simulation_DIP(
  scenario = scenario1,
  n_sims = 1000,
  target = 0.30
)

summary_results(results)
```

To switch to posterior mode estimation, modify the DIP function setting before execution.

---

## Reproducibility

All manuscript results can be reproduced using:

1. `single_agent_CDP_simulations_v2` # for non-flat prior
2. `Single-agent CDP simulations*.R`
3. `Single-agent CDP -DIP-simulations.R`

R version and package information are provided in `session_info.txt`.

---

## Citation

If you use this code, please cite:

()

---

## Contact

Darya Shokouhi Niaki  
Ph.D. Student in Biostatistics  
Virginia Commonwealth University  

For questions or collaboration inquiries, please open an issue.
