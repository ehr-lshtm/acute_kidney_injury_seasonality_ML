### AKI code in any diagnostic position, any point of admisision, from 2015

# Run muliple correspodence analysis

cprd_aki_clinical <- cprd_aki_clinical_any_2015

source("05_mca.R")

cprd_aki_clinical <- cprd_aki_clinical_any_2015
g_hes_aki_spell <- g_hes_aki_spell_any_2015

# Run figures for paper including k-means clustering 

rmarkdown::render("07_figures.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("figures", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

# Run supplementary figures for paper

rmarkdown::render("09_supplement_figures.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("supplement_figures", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

# Run supplementary tables for paper

rmarkdown::render("09_supplement_tables.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("supplement_figures", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

#########################