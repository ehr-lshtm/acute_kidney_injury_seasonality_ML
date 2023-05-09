# Purpose
This is the project code for the paper 'Seasonality of acute kidney injury phenotypes in England: an unsupervised machine learning classification study of electronic health records'

# Untracked files

This repository contains only non-disclosive files, that is, code without file paths, and summary statistics. This template is set up so only files that are safe to upload to Github, such as code, are uploaded by default.

# File tree

```
template-r/
├── codelists/
│   ├── ICD10_codes_list_v2.txt
│   ├── ICD10_look_up_codes.csv
│   ├── readcode_lookup.txt
|   └── README.md
├── R/
│   ├── 00_filepath.R (untracked)
│   ├── 01_setup.R
│   ├── 02_functions.R
│   ├── 03_data_prep.R
│   ├── 04_data_prep_cohorts.R
│   ├── 05_mca.R
│   ├── 06_kmeans_run.R
│   ├── 07_figures.R
│   ├── 08_kmeans_results_chapter3.R
│   ├── 09_supplement_figures.R
│   ├── 09_supplement_tables.R
│   ├── 10_optimum_clusters.R
│   ├── 11_sensitivity_analysis_diagnostic_day.R
│   ├── 11_sensitivity_analysis_diagnostic_position.R
│   ├── 12_mca_sensitivity.R
│   ├── 13_figures_sensitivity.R
│   ├── aki_infections_30_days.R
│   ├── load_data_sensitivity.R
│   ├── load_data.R
│   ├── README.md
│   └── run.R
├── results/
│   └── EADME.md
├── aki_seasonality_ML.Rproj
└── README.md
```

# File description summary in codelist folder

ICD10_codes_list_v2.txt
- ICD10 codes list used to define acute kidney injury

ICD10_look_up_codes.csv
- ICD10 look up to summarise hospital admission data

readcode_lookup.txt
- Read code look up to summarise primary care data for multiple correspondence analysis and k means clustering

# Code description summary in R folder

00_filepath.R
- contains file paths and not included due to sensitivity

01_setup.R
- contains packages used, and colour palettes used

02_functions.R
- contains functions used through analysis

03_data_prep.R
- data preparation script to define acute kidney injury cohort by unique hospitalisations, processing data variables, and creating new categorical variables

04_data_prep_cohorts.R
- loading AKI cohort and linking to primary care data (cprd_clinical)

05_mca.R
- processing primary care data of AKI cohort, converting records to a indicator matrix, and applying multiple correspondence analysis to summarise data 

06_kmeans_run.R
- summary script that sets the cohort, runs multiple correspondence analysis script, renders rmarkdown html of kmeans clustering and paper figures, renders rmakrdown html of supplementary figures, and renders rmarkdown of word document of supplementary figures and tables

07_figures.R
- a script to render a rmarkdown html file running kmeans clustering and producing figures 1 to 4

08_kmeans_results_chapter3.R
- a script used in 07_figures.R to summarise (by percentage reported) each Read code in the cohort by Read code level 3 and level 2 (used to describe the cluster composition) 

09_supplement_figures.R
- a script to render a rmarkdown html file producing supplementary figures

09_supplement_tables.R
- a script to render a rmarkdown word file producing supplementary figures and tables

10_optimum_clusters.R
- a script to render a rmarkdown html file running the optimum cluster analysis using NbClust()

11_sensitivity_analysis_diagnostic_day.R
- a script to separating the cohort by diagnostic day of AKI recorded during hospitalisation, and rendering rmarkdown html scripts for multiple correspondence analysis and kmeans clustering of newly defined cohorts by diagnostic day

11_sensitivity_analysis_diagnostic_position.R
- a script to separating the cohort by diagnostic position of AKI recorded during hospitalisation, and rendering rmarkdown html scripts for multiple correspondence analysis and kmeans clustering of newly defined cohorts by diagnostic position

12_mca_sensitivity.R
- a script used in 11_sensitivity_analysis_diagnostic_day.R and 11_sensitivity_analysis_diagnostic_position.R to run multiple correspondence analysis for sensitivity analysis

13_figures_sensitivity.R
- a script used in 11_sensitivity_analysis_diagnostic_day.R and 11_sensitivity_analysis_diagnostic_position.R to render rmarkdown html for figures produced in sensitivity analysis

aki_infections_30_days.R
- script used in 05_mca.R to restrict infection codes reported in primary care to those reported in 30 days prior to AKI admission

load_data_sensitivity.R
- a script used in 11_sensitivity_analysis_diagnostic_day.R and 11_sensitivity_analysis_diagnostic_position.R to load data for sensitivity analysis

load_data.R
- a script used in 07_figures.R to load data for kmeans clustering analysis
