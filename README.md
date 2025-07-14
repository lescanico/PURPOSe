# PURPOSe: Predicting Utilization of Resources in Psychiatry Outpatient Services

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-%3E=4.0-blue.svg)](https://cran.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-Enabled-blueviolet)](https://quarto.org/)

**Author:** Nicolas Lescano, MD  
Professor of Clinical Psychiatry  
University of Pennsylvania

---

## Overview
PURPOSe is a reproducible data science pipeline for predicting outpatient psychiatry resource utilization using structured EHR data. The project leverages machine learning to forecast annual provider time utilization (hours per year, HPY) and classify patients into high-utilization tiers (HUT), supporting operational planning and patient care.

## Features
- **End-to-end pipeline:** Data anonymization, cleaning, feature engineering, modeling, and reporting
- **Robust, interpretable features:** For clinical and operational use
- **Reproducible environment:** Managed with `renv` and Quarto
- **Data privacy:** All analyses use de-identified data; raw data is never committed
- **Extensible:** Adaptable for other healthcare resource prediction tasks

## Setup
1. **Install R (>= 4.0) and RStudio or VS Code**
2. **Clone this repository**
3. **Install Quarto** ([instructions](https://quarto.org/docs/get-started/))
4. **Open the project in your IDE**
5. **Install required R packages**:
   - The pipeline uses `renv` for reproducibility. In R, run:
     ```r
     if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
     renv::restore()
     ```
6. **Run the pipeline**:
   - Render the Quarto document: `quarto render pipeline.qmd`
   - Or run code chunks interactively in your IDE

## Usage
- Place raw data in the `secure/` directory as described in the pipeline (this folder is gitignored for privacy).
- Run the pipeline to generate anonymized, cleaned, and feature-engineered datasets, as well as model outputs and figures.
- See `pipeline.qmd` for detailed documentation and code.

## Data Dictionary
A brief overview of key variables used in the pipeline:

| Variable         | Description                                      |
|------------------|--------------------------------------------------|
| patient_id       | Anonymized patient identifier                    |
| provider_id      | Anonymized provider identifier                   |
| date             | Visit date (YYYY-MM-DD)                          |
| time             | Appointment time (decimal hours)                 |
| length           | Appointment length (minutes)                     |
| type             | Visit type (e.g., New, Follow up, Other)         |
| status           | Appointment status (e.g., Completed, No Show)    |
| age              | Patient age at visit (years)                     |
| sex              | Legal sex                                        |
| race             | Patient race                                     |
| diagnosis        | Primary diagnosis code or label                  |
| medications      | Medications ordered                              |
| ...              | See `pipeline.qmd` and scripts for full details  |

## Example Data
For privacy, no real patient data is included. If you wish to test the pipeline, you may request a fully anonymized example dataset or generate synthetic data using the provided scripts.

## Reproducibility
- All package versions are managed with `renv`.
- The analysis is fully scripted and documented in `pipeline.qmd`.
- Outputs are saved in `data/`, `figures/`, and `tables/` directories.

## License
This project is licensed under the [MIT License](LICENSE).

## Contact
For questions, collaboration, or data requests, contact:

**Nicolas Lescano, MD**  
Professor of Clinical Psychiatry  
University of Pennsylvania  
Email: nlescano@upenn.edu 