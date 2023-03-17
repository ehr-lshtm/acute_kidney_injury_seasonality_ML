# infection read codes

infection_codes <- c("A", "H0", "H1", "H2", "K190")

# identify infection codes of interest

cprd_aki_infection_codes <- cprd_aki_clinical %>%
  mutate(eventdate = dmy(eventdate),
         year = year(eventdate)) %>%
  left_join(medcode_lookup, by = "medcode") %>%
  filter(
    chapter_read %in% "A" |
    chapter_read2 %in% "H0" |
    chapter_read2 %in% "H1" |
    chapter_read2 %in% "H2" |
    chapter_read4 %in% "K190"
  ) %>%
  print()

## UPDATE

aki_infections_30days <- g_hes_aki_spell %>%
  distinct(patid, spno, admidate, group) %>%
  plyr::join(cprd_aki_infection_codes, by = "patid", match = "all") %>%
  mutate(admidate = ymd(admidate),
         eventdate = ymd(eventdate),
         date_diff = admidate - eventdate) %>%
  filter(date_diff <= 30 & date_diff >= 0)  %>%
  distinct(patid, eventdate, medcode, adid) %>%
  left_join(medcode_lookup, by = "medcode") %>% 
  print()



