## load data

#### load data

medcode_lookup <-
  fread(file.path(data_info_path, "Look up files/Lookups_2022_01/medical.txt")) %>% 
  mutate(
    chapter_read = str_sub(readcode, 1, 1),
    chapter_read2 = str_sub(readcode, 1, 2),
    chapter_read3 = str_sub(readcode, 1, 3),
    chapter_read4 = str_sub(readcode, 1, 4),
    chapter_read5 = str_sub(readcode, 1, 5),
    term_code = str_sub(readcode, 6, 7)
  )

readcode_lookup <- fread("data/readcode_lookup.txt")

icd10_lookup <- fread("data/ICD10_look_up_codes.csv") %>% 
  rename("category_code" = "V1",
         "diagnosis_code" = "V2",
         "full_code" = "V3",
         "abbrev_desc" = "V4",
         "full_desc" = "V5",
         "category_title" = "V6") %>%
  mutate(ICD3 = substr(category_code, 1, 3))


cprd_aki_patient <-
  read_dta(file.path(
    rawdata_cprd_files_path,
    "Patient_extract_gold_akicohort_1.dta"
  ))

aki_cohort_chapter3 <-
  fread(file.path(rawdata_temp_files_path, "aki_cohort_chapter3.txt"))          

aki_cohort_chapter2 <-
  fread(file.path(rawdata_temp_files_path, "aki_cohort_chapter2.txt"))          

ind.coord <-
  fread(file.path(rawdata_temp_files_path, "ind_coord_chapter3_600.txt"))            

res.mca <-
  readRDS("res_mca_chapter3_600.RData")                                              

g_hes_aki_spell_full <-
  fread(file.path(data_files_path, "g_hes_aki_spell.txt"),
        showProgress = TRUE) %>% 
  mutate(
    isoweek = isoweek(admidate),
    isoyear = isoyear(admidate),
    epiweek = epiweek(admidate),
    epiyear = epiyear(admidate)) %>%
  filter(admidate > "2015-01-03" & admidate < "2019-12-29")                                      # epi week 1-2015 starts 2015-01-04 AND epi week 52-2019 ends 2019-12-28  


g_hes_diagnosis_epi <-
  fread(file.path(rawdata_hes_files_path, "hes_diagnosis_epi_21_000715_request4_DM.txt"), showProgress = TRUE
  )

g_primary_diag_hosp <-
  fread(file.path(rawdata_hes_files_path, "hes_primary_diag_hosp_21_000715_request4_DM.txt"), showProgress = TRUE
  )

### rename variables

aki_first_episode_date <- g_hes_aki_spell %>% 
  mutate(admiyear = year(admidate),
         patid = as.numeric(patid)) %>% 
  arrange(patid, admidate) %>% 
  group_by(patid) %>%
  mutate(episode_num = 1:n()) %>% 
  ungroup() %>%
  filter(episode_num == 1) %>% 
  select(patid, admiyear)

aki_cohort_demograph <- aki_cohort_chapter3 %>%
  mutate(patid = as.numeric(patid)) %>% 
  left_join(cprd_aki_patient %>% select(patid, gender, realyob, eth5), by = "patid") %>%
  left_join(aki_first_episode_date, by = "patid") %>% 
  mutate(
    age = admiyear - realyob,
    gender = case_when(gender == 1 ~ "Male",
                       gender == 2 ~ "Female"),
    age_group = case_when(
      age >= 0 & age < 10 ~ "0-9",
      age >= 10 & age < 20 ~ "10-19",
      age >= 20 & age < 30 ~ "20-29",
      age >= 30 & age < 40 ~ "30-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 & age < 60 ~ "50-59",
      age >= 60 & age < 70 ~ "60-69",
      age >= 70 & age < 80 ~ "70-79",
      age >= 80 ~ "80+",
    ),
    ethnicity = case_when(
      eth5 == 0 ~ "White",
      eth5 == 1 ~ "South Asian",
      eth5 == 2 ~ "Black",
      eth5 == 3 ~ "Other",
      eth5 == 4 ~ "Mixed",
      eth5 == 5 ~ "Not stated"
    ),
    ethnicity = factor(
      ethnicity,
      c("White", "South Asian", "Black", "Other", "Mixed", "Not stated")
    )
  )
