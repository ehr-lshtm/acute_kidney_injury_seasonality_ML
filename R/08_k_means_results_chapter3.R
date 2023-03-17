# calculate percentage of characharacteristics of each cluster chapter 3

table_chapter_read3_percentage <- aki_cohort_chapter3 %>%
  select(cluster, everything(), -patid) %>%
  group_by(cluster) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  select(cluster, total, everything()) %>%
  pivot_longer(
    cols =  3:ncol(.) ,
    names_to = "chapter_read3",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>%
  left_join(readcode_lookup %>% 
              count(chapter_read, chapter_read2, chapter_read3, desc_chapter1, desc_chapter2, desc_chapter3) %>%                         # THIS NEEDS EDITING - NOT A GOOD WAY TO DO IT
              select(-n),
            by = "chapter_read3") %>% 
  mutate(desc_chapter3 = case_when(is.na(desc_chapter3) ~ coalesce(desc_chapter2, desc_chapter1), TRUE ~ desc_chapter3))

chapter_read3_percentage <- table_chapter_read3_percentage %>% 
  group_by(cluster) %>% 
  count(cluster, chapter_read3, desc_chapter3, total) %>%
  mutate(freq = round((n/total)*100, 0)) %>% 
  ungroup()

chapter_read3_percentage_overall <- aki_cohort_chapter3 %>%
  mutate(total = n()) %>% 
  select(total, everything(), -patid, - cluster) %>%
  pivot_longer(
    cols =  2:ncol(.) ,
    names_to = "chapter_read3",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>%
  left_join(readcode_lookup %>% 
              count(chapter_read, chapter_read2, chapter_read3, desc_chapter1, desc_chapter2, desc_chapter3) %>%                         # THIS NEEDS EDITING - NOT A GOOD WAY TO DO IT
              select(-n),
            by = "chapter_read3") %>% 
  mutate(desc_chapter3 = case_when(is.na(desc_chapter3) ~ coalesce(desc_chapter2, desc_chapter1), TRUE ~ desc_chapter3)) %>% 
  count(chapter_read3, desc_chapter3, total) %>%
  mutate(freq = round((n/total)*100, 0)) 

# calculate percentage of characharacteristics of each cluster chapter 3

table_chapter_read2_percentage <- aki_cohort_chapter2 %>%
  select(cluster, everything(), -patid) %>%
  group_by(cluster) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  select(cluster, total, everything()) %>%
  pivot_longer(
    cols =  3:ncol(.) ,
    names_to = "chapter_read2",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>%
  left_join(readcode_lookup %>% 
              count(chapter_read, chapter_read2, desc_chapter1, desc_chapter2) %>%                         # THIS NEEDS EDITING - NOT A GOOD WAY TO DO IT
              select(-n),
            by = "chapter_read2") %>% 
  mutate(
    desc_chapter2 = case_when(is.na(desc_chapter2) ~ coalesce(desc_chapter1), TRUE ~ desc_chapter2))

chapter_read2_percentage <- table_chapter_read2_percentage %>% 
  group_by(cluster) %>% 
  count(cluster, chapter_read2, desc_chapter2, total) %>%
  mutate(freq = round((n/total)*100, 0)) %>% 
  ungroup()

chapter_read2_percentage_overall <- aki_cohort_chapter2 %>%
  mutate(total = n()) %>% 
  select(total, everything(), -patid, - cluster) %>%
  pivot_longer(
    cols =  2:ncol(.) ,
    names_to = "chapter_read2",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>%
  left_join(readcode_lookup %>% 
              count(chapter_read, chapter_read2, desc_chapter1, desc_chapter2) %>%                         # THIS NEEDS EDITING - NOT A GOOD WAY TO DO IT
              select(-n),
            by = "chapter_read2") %>% 
  mutate(
    desc_chapter2 = case_when(is.na(desc_chapter2) ~ coalesce(desc_chapter1), TRUE ~ desc_chapter2)) %>% 
  count(chapter_read2, desc_chapter2, total) %>%
  mutate(freq = round((n/total)*100, 0),
         cluster = "cohort")

