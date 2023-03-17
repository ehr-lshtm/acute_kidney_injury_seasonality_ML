## ML reshaping

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

diagnosis_chapters <- c("A", "B", "C", "D", "E", "F", "G", "H","I", "J",
                        "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                        "U", "V", "W", "X", "Y") # exlude "Z"

# load in aki_infection codes

source("aki_infections_30days.R")

# limit to chapter codes of interest and aki infections codes

cprd_aki_clinical <- cprd_aki_clinical %>%
  left_join(medcode_lookup, by = "medcode") %>%
  filter(
    chapter_read %in% diagnosis_chapters,
    !chapter_read %in% "A",
    !chapter_read2 %in% "H0",
    !chapter_read2 %in% "H1",                  
    !chapter_read2 %in% "H2",
    !chapter_read4 %in% "K190"
  ) %>%
  mutate(
    eventdate = dmy(eventdate),
    year = year(eventdate)) %>%
  select(-term_code) %>% 
  bind_rows(aki_infections_30days)

# run medcode_count.R from here if you want to check how many medcodes are included in the dataset and covers how much of the data

###  

medcode_count <- cprd_aki_clinical %>%
  distinct(patid, medcode, chapter_read3) %>%               #### change to chapters or medcode up the hierarchy
  group_by(chapter_read3) %>%                               #### change to chapters or medcode up the hierarchy
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(n >= 100)

### aki cohort transformation for counting chapters

aki_cohort <- cprd_aki_clinical %>%
  inner_join(medcode_count, by = "chapter_read3")               #### change to chapters or medcode up the hierarchy

aki_cohort_chapter3 <- aki_cohort %>%
  distinct(patid, chapter_read3) %>% 
  mutate(include_medcode = 1) %>% 
  pivot_wider(names_from = chapter_read3, values_from = include_medcode)  %>% 
  write_tsv(file.path(rawdata_temp_files_path, "aki_cohort_chapter3.txt"))  

aki_cohort_chapter2 <- aki_cohort %>%
  distinct(patid, chapter_read2) %>% 
  mutate(include_medcode = 1) %>% 
  pivot_wider(names_from = chapter_read2, values_from = include_medcode) %>%  
  write_tsv(file.path(rawdata_temp_files_path, "aki_cohort_chapter2.txt"))  

### aki cohort transformation for mca

mca_data <- aki_cohort_chapter3 %>%
  select(-patid) %>% 
  mutate(across(everything(), ~ ifelse(
    is.na(.x),
    paste("n", cur_column(), sep = "_"),
    paste("y", cur_column(), sep = "_")
  ))) %>% 
  mutate_if(is.character,as.factor) %>%
  as.matrix()

remove(cprd_aki_clinical)
remove(medcode_count)
remove(medcode_lookup)
remove(aki_cohort)
gc()

# Multiple correspondence analysis

res.mca <- MCA(mca_data, graph = FALSE, ncp = 600)                                  

saveRDS(res.mca, file="res_mca_chapter3_600.RData")                                  #### change to chapters or medcode up the hierarchy

## Compute and extract individual coordinates

ind.coord <- as.data.frame(get_mca_ind(res.mca)$coord)

ind.coord %>% 
  write_tsv(file.path(rawdata_temp_files_path, "ind_coord_chapter3_600.txt"))          #### change to chapters or medcode up the hierarchy
