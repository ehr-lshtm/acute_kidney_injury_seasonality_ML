###### sensitivity analysis changing diagnostic position

### sensitivity analysis

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

# AKI ICD-10 code day 0 admission from 2015 onwards

g_hes_aki_spell_day_0_1 <- g_hes_aki_spell %>%
  filter(admidate > "2014-12-28") %>%
  mutate(isoweek = isoweek(admidate),
         isoyear = isoyear(admidate)) %>%
  filter(aki_day < 2,
         isoweek != "1" & isoyear != "2020") %>% 
  distinct(patid, spno, admidate, gender, yob, group)

cprd_aki_clinical_day_0_1 <- cprd_aki_clinical %>% 
  left_join(g_hes_aki_spell_day_0_1 %>% distinct(patid) %>% mutate(include = 1), by = "patid") %>% 
  filter(include == 1) %>% 
  select(-include)

# AKI ICD-10 code day 2 plus admission from 2015 onwards

g_hes_aki_spell_day_2_plus <- g_hes_aki_spell %>%
  filter(admidate > "2014-12-28") %>%
  mutate(isoweek = isoweek(admidate),
         isoyear = isoyear(admidate)) %>%
  filter(aki_day > 1,
         isoweek != "1" & isoyear != "2020") %>% 
  distinct(patid, spno, admidate, gender, yob, group)

cprd_aki_clinical_day_2_plus <- cprd_aki_clinical %>% 
  left_join(g_hes_aki_spell_day_2_plus %>% distinct(patid) %>% mutate(include = 1), by = "patid") %>% 
  filter(include == 1) %>% 
  select(-include)

####################### Running analysis

# AKI ICD-10 code day 0 admission from 2015 onwards

source("12_mca_sensitivity.R")

cprd_aki_clinical <- cprd_aki_clinical_day_0_1
g_hes_aki_spell <- g_hes_aki_spell_day_0_1

rmarkdown::render("13_figures_sensitivity.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("sensitivity_analysis_day_0_1_2015", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

# AKI ICD-10 code day 2 plus admission from 2015 onwards

cprd_aki_clinical <- cprd_aki_clinical_day_2_plus

source("12_mca_sensitivity.R")

cprd_aki_clinical <- cprd_aki_clinical_day_2_plus
g_hes_aki_spell <- g_hes_aki_spell_day_2_plus

rmarkdown::render("13_figures_sensitivity.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("sensitivity_analysis_day_2_plus_2015", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

###########################

