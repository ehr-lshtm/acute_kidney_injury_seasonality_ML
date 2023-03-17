###### sensitivity analysis changing diagnostic position

### cohorts

cprd_aki_clinical <-
  fread(
    "Z:/GPRD_GOLD/Hikaru/CPRD/rawdata/4. gold_extract/aki/Clinical_extract_gold_akicohort_1.txt"
  )

g_hes_aki_spell <-
  fread(
    "Z:/GPRD_GOLD/Hikaru/CPRD/datafiles/Gold/g_hes_aki_spell.txt", showProgress = TRUE) %>% 
  filter(year > 2008)

########################### Prepare cohorts

# AKI ICD-10 code the first or second diagnostic position, from 2015 onwards

g_hes_aki_spell_d1 <- g_hes_aki_spell %>%
  mutate(epiweek = epiweek(admidate),
         epiyear = epiyear(admidate)) %>%
  filter(d_order == 1,
         admidate > "2015-01-03" & admidate < "2019-12-29") %>% 
  distinct(patid, spno, admidate, gender, yob, group, epiweek, epiyear)
  
cprd_aki_clinical_d1 <- cprd_aki_clinical %>% 
  inner_join(g_hes_aki_spell_d1 %>% distinct(patid), by = "patid")

# AKI ICD-10 code the third diagnostic position or greater, from 2015 onwards

g_hes_aki_spell_d2_plus <- g_hes_aki_spell %>%
  mutate(epiweek = epiweek(admidate),
         epiyear = epiyear(admidate)) %>%
  filter(d_order >1,
         admidate > "2015-01-03" & admidate < "2019-12-29") %>% 
  distinct(patid, spno, admidate, gender, yob, group, epiweek, epiyear)

cprd_aki_clinical_d2_plus <- cprd_aki_clinical %>% 
  inner_join(g_hes_aki_spell_d2_plus %>% distinct(patid), by = "patid")

####################### Running analysis

# AKI ICD-10 code the first or second diagnostic position, from 2015 onwards

cprd_aki_clinical <- cprd_aki_clinical_d1

source("12_mca_sensitivity.R")

cprd_aki_clinical <- cprd_aki_clinical_d1
g_hes_aki_spell <- g_hes_aki_spell_d1

rmarkdown::render("13_figures_sensitivity.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("sensitivity_analysis_d1_", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

# AKI ICD-10 code the third diagnostic position or greater, from 2015 onwards

cprd_aki_clinical <- cprd_aki_clinical_d2_plus

source("12_mca_sensitivity.R")

cprd_aki_clinical <- cprd_aki_clinical_d2_plus
g_hes_aki_spell <- g_hes_aki_spell_d2_plus

rmarkdown::render("13_figures_sensitivity.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("sensitivity_analysis_d2_plus_", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))
