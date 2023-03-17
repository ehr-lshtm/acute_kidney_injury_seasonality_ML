### load data

cprd_aki_clinical <- fread(
  file.path(rawdata_cprd_files_path, "Clinical_extract_gold_akicohort_1.txt"),
  showProgress = TRUE)

g_hes_aki_spell <-
  fread(file.path(data_files_path, "g_hes_aki_spell.txt"),
        showProgress = TRUE)

# AKI ICD-10 code in any diagnostic position during the admission, from 2015 onwards

g_hes_aki_spell_any_2015 <- g_hes_aki_spell %>%
  mutate(epiweek = epiweek(admidate),
         epiyear = epiyear(admidate)) %>%
  distinct(patid, spno, admidate, gender, yob, group, epiweek, epiyear) %>%
  filter(admidate > "2015-01-03" &
           admidate < "2019-12-29")                                      

cprd_aki_clinical_any_2015 <- cprd_aki_clinical %>%
  inner_join(g_hes_aki_spell_any_2015 %>% distinct(patid),
             by = "patid")
