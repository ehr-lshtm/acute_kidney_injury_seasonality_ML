# data icd 10 codes

icd_10_codes <- fread("data/ICD10_codes_list_v2.txt")

# data prepartion gold

all_pat_denominator_gold <-
  fread(file.path(data_info_path, "Denominator files/JUL2021/all_patients_JUL2021.txt"),  showProgress = TRUE) %>% 
  select(patid, gender, yob)

# date on admission data and duration

g_hes_hospital <-
  fread(file.path(rawdata_hes_files_path, "hes_hospital_21_000715_request4_DM.txt"), showProgress = TRUE)

g_hes_diagnosis_epi <-
  fread(file.path(rawdata_hes_files_path, "hes_diagnosis_epi_21_000715_request4_DM.txt"), showProgress = TRUE)

# hes AKI

g_hes_aki_spell <- g_hes_diagnosis_epi %>%
  left_join(icd_10_codes, by = c("ICD" = "icd")) %>%
  filter(group == "Acute Kidney Injury") %>%
  left_join(all_pat_denominator_gold, by = "patid") %>%                         # using gender and yob from CPRD
  distinct(patid, spno, epistart, gender, yob, group, d_order) %>%
  left_join(g_hes_hospital, by = c("patid", "spno")) %>%                        # including g_hes_hospital for admission date
  mutate(epistart = dmy(epistart),
         admidate = dmy(admidate),
         discharged = dmy(discharged),
         admiyear = year(admidate),
         aki_day = epistart - admidate,
         age_at_admission = year - yob,
         age_group = case_when(
           age_at_admission <65 ~ "<65",
           age_at_admission >= 65 & age_at_admission <= 74 ~ "65-74",
           age_at_admission >= 75 & age_at_admission <= 84 ~ "75-84",
           age_at_admission >= 85 ~ ">84"),
         age_group = factor(age_group, levels = c(">84", "75-84", "65-74", "<65")),
         gender = case_when(
           gender == 1 ~ "Male",
           gender == 2 ~ "Female"
         )) %>%
  write_tsv(file.path(data_files_path, "g_hes_aki_spell.txt"))